#![allow(non_snake_case)]

use libc::{free, malloc, wcslen};
use std::{fs, ptr};
use widestring::{u16cstr, u16str, U16CStr, U16CString, U16String};
use winapi::{
    ctypes::wchar_t,
    shared::{
        guiddef::GUID,
        minwindef::{DWORD, HKEY, HKEY__, LPFILETIME},
        ntdef::{LCID, LPCWSTR},
        winerror::{ERROR_MORE_DATA, HRESULT, S_OK},
        wtypes::BSTR,
        wtypesbase::{CLSCTX_INPROC_SERVER, LPCOLESTR, ULONG},
    },
    um::{
        combaseapi::{CoCreateInstance, CoInitializeEx},
        fileapi::{FindClose, FindFirstFileW, FindNextFileW, GetFileAttributesW, INVALID_FILE_ATTRIBUTES},
        handleapi::INVALID_HANDLE_VALUE,
        minwinbase::WIN32_FIND_DATAW,
        objbase::COINIT_MULTITHREADED,
        oleauto::SysFreeString,
        unknwnbase::{IUnknown, IUnknownVtbl},
        winnt::{FILE_ATTRIBUTE_DIRECTORY, KEY_ENUMERATE_SUB_KEYS, KEY_QUERY_VALUE, KEY_WOW64_32KEY, REG_SZ},
        winreg::{RegCloseKey, RegOpenKeyExA, RegQueryValueExW, HKEY_LOCAL_MACHINE},
    },
    RIDL,
};

struct Defer<R, F: FnOnce() -> R>(Option<F>);

impl<R, F: FnOnce() -> R> Drop for Defer<R, F> {
    fn drop(&mut self) {
        self.0.take().map(|f| f());
    }
}

pub fn defer<R, F: FnOnce() -> R>(f: F) -> impl Drop {
    Defer(Some(f))
}

#[derive(Debug, Clone)]
pub struct FindResult {
    pub windows_sdk_version: i32, // Zero if no Windows SDK found.
    pub windows_sdk_root: Option<U16String>,
    pub windows_sdk_um_library_path: Option<U16String>,
    pub windows_sdk_ucrt_library_path: Option<U16String>,
    pub vs_exe_path: Option<U16String>,
    pub vs_library_path: Option<U16String>,
}

RIDL! {#[uuid(0xb41463c3, 0x8866, 0x43b5, 0xbc, 0x33, 0x2b, 0x06, 0x76, 0xf7, 0xf4, 0x2e)]
interface ISetupInstance(ISetupInstanceVtbl): IUnknown(IUnknownVtbl) {
    fn GetInstanceId(
        pbstrInstanceId: *mut BSTR,
    ) -> HRESULT,
    fn GetInstallDate(
        pInstallDate: LPFILETIME,
    ) -> HRESULT,
    fn GetInstallationName(
        pbstrInstallationName: *mut BSTR,
    ) -> HRESULT,
    fn GetInstallationPath(
        pbstrInstallationPath: *mut BSTR,
    ) -> HRESULT,
    fn GetInstallationVersion(
        pbstrInstallationVersion: *mut BSTR,
    ) -> HRESULT,
    fn GetDisplayName(
        lcid: LCID,
        pbstrDisplayName: *mut BSTR,
    ) -> HRESULT,
    fn GetDescription(
        lcid: LCID,
        pbstrDescription: *mut BSTR,
    ) -> HRESULT,
    fn ResolvePath(
        pwszRelativePath: LPCOLESTR,
        pbstrAbsolutePath: *mut BSTR,
    ) -> HRESULT,
}}

RIDL! {#[uuid(0x6380bcff, 0x41d3, 0x4b2e, 0x8b, 0x2e, 0xbf, 0x8a, 0x68, 0x10, 0xc8, 0x48)]
interface IEnumSetupInstances(IEnumSetupInstancesVtbl) : IUnknown(IUnknownVtbl) {
    fn Next(
        celt: ULONG,
        rgelt: *mut *mut ISetupInstance,
        pceltFetched: *mut ULONG,
    ) -> HRESULT,
    fn Skip(
        celt: ULONG,
    ) -> HRESULT,
    fn Reset() -> HRESULT,
    fn Clone(
        ppenum: *mut *mut IEnumSetupInstances,
    ) -> HRESULT,
}}

RIDL! {#[uuid(0x42843719, 0xdb4c, 0x46c2, 0x8e, 0x7c, 0x64, 0xf1, 0x81, 0x6e, 0xfd, 0x5b)]
interface ISetupConfiguration(ISetupConfigurationVtbl): IUnknown(IUnknownVtbl) {
    fn EnumInstances(
        ppEnumInstances: *mut *mut IEnumSetupInstances,
    ) -> HRESULT,
    fn GetInstanceForCurrentProcess(
        ppInstance: *mut *mut ISetupInstance,
    ) -> HRESULT,
    fn GetInstanceForPath(
        wzPath: LPCWSTR,
        ppInstance: *mut *mut ISetupInstance,
    ) -> HRESULT,
}}

struct VersionData {
    best_version: [i32; 4],
    best_name: U16String,
}

fn os_file_exists(name: *const wchar_t) -> bool {
    let attr = unsafe { GetFileAttributesW(name) };
    attr != INVALID_FILE_ATTRIBUTES && attr & FILE_ATTRIBUTE_DIRECTORY == 0
}

type VisitFn = unsafe fn(&U16CStr, U16String, &mut VersionData);

unsafe fn visit_files_w(dir_name: U16String, data: &mut VersionData, f: VisitFn) -> bool {
    let mut wildcard_name = dir_name.clone();
    wildcard_name.push(u16str!("\\*"));
    let wildcard_name = U16CString::from_ustr(wildcard_name).unwrap();

    let mut find_data = WIN32_FIND_DATAW::default();

    let handle = FindFirstFileW(wildcard_name.as_ptr(), &mut find_data);

    let dot = u16str!(".").as_slice()[0];

    if handle != INVALID_HANDLE_VALUE {
        loop {
            if (find_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY != 0) && (find_data.cFileName[0] != dot) {
                let short_name =
                    U16CStr::from_ptr(find_data.cFileName.as_ptr(), wcslen(find_data.cFileName.as_ptr())).unwrap();

                let mut full_name = dir_name.clone();
                full_name.push(u16str!("\\"));
                full_name.push(short_name);

                f(short_name, full_name, data);
            }

            if FindNextFileW(handle, &mut find_data) == 0 {
                break;
            }
        }

        FindClose(handle);

        true
    } else {
        false
    }
}

unsafe fn read_from_the_registry(key: HKEY, value_name: *const wchar_t) -> Option<Box<wchar_t>> {
    let mut required_length: DWORD = 0;

    if RegQueryValueExW(
        key,
        value_name,
        ptr::null_mut(),
        ptr::null_mut(),
        ptr::null_mut(),
        &mut required_length,
    ) == 0
    {
        #[allow(unused_assignments)]
        let mut value = ptr::null_mut::<wchar_t>();

        #[allow(unused_assignments)]
        let mut length: DWORD = 0;

        loop {
            length = required_length + 2; // The +2 is for the maybe optional zero later on. Probably we are over-allocating.
            value = malloc(length as usize + 2) as _; // This second +2 is for crazy situations where there are race conditions or the API doesn't do what we want!

            if value.is_null() {
                return None;
            }

            let mut type_: DWORD = 0;

            // We know that version is zero-terminated...
            let rc = RegQueryValueExW(key, value_name, ptr::null_mut(), &mut type_, value as _, &mut length);
            if rc == ERROR_MORE_DATA as i32 {
                free(value as _);
                required_length = length;
                continue;
            }

            if rc != S_OK || type_ != REG_SZ {
                // REG_SZ because we only accept strings here!
                free(value as _);
                return None;
            }

            break;
        }

        // If the string was already zero-terminated, this just puts an extra 0 after (since that 0 was counted in 'length').
        // If it wasn't, this puts a 0 after the nonzero characters we got.
        let num_wchars = length / 2;
        *value.add(num_wchars as _) = 0;

        if value.is_null() {
            None
        } else {
            Some(Box::from_raw(value))
        }
    } else {
        return None;
    }
}

unsafe fn win10_best(short_name: &U16CStr, full_name: U16String, data: &mut VersionData) {
    let parts = short_name
        .to_string()
        .unwrap()
        .split('.')
        .map(|part| part.parse::<i32>().unwrap())
        .collect::<Vec<_>>();

    if let [i0, i1, i2, i3] = &parts[..] {
        let (i0, i1, i2, i3) = (*i0, *i1, *i2, *i3);

        if i0 < data.best_version[0] {
            return;
        } else if i0 == data.best_version[0] {
            if i1 < data.best_version[1] {
                return;
            } else if i1 == data.best_version[1] {
                if i2 < data.best_version[2] {
                    return;
                } else if i2 == data.best_version[2] {
                    if i3 < data.best_version[3] {
                        return;
                    }
                }
            }
        }

        // we have to copy_string and free here because visit_files free's the full_name string
        // after we execute this function, so Win*_Data would contain an invalid pointer.
        data.best_name = full_name;

        data.best_version[0] = i0;
        data.best_version[1] = i1;
        data.best_version[2] = i2;
        data.best_version[3] = i3;
    }
}

unsafe fn win8_best(short_name: &U16CStr, full_name: U16String, data: &mut VersionData) {
    let parts = short_name
        .to_string()
        .unwrap()
        .trim_start_matches("winv")
        .split('.')
        .map(|part| part.parse::<i32>().unwrap())
        .collect::<Vec<_>>();

    if let [i0, i1] = &parts[..] {
        let (i0, i1) = (*i0, *i1);

        if i0 < data.best_version[0] {
            return;
        } else if i0 == data.best_version[0] {
            if i1 < data.best_version[1] {
                return;
            }
        }

        // we have to copy_string and free here because visit_files free's the full_name string
        // after we execute this function, so Win*_Data would contain an invalid pointer.
        data.best_name = full_name;

        data.best_version[0] = i0;
        data.best_version[1] = i1;
    }
}

struct SafeHKey {
    inner: HKEY,
}

impl SafeHKey {
    fn new() -> Self {
        Self {
            inner: ptr::null_mut::<HKEY__>(),
        }
    }
}

impl Drop for SafeHKey {
    fn drop(&mut self) {
        if !self.inner.is_null() {
            unsafe {
                RegCloseKey(self.inner);
            }
        }
    }
}

unsafe fn find_windows_kit_root(result: &mut FindResult) {
    // Information about the Windows 10 and Windows 8 development kits
    // is stored in the same place in the registry. We open a key
    // to that place, first checking preferntially for a Windows 10 kit,
    // then, if that's not found, a Windows 8 kit.

    let mut main_key = SafeHKey::new();

    let rc = RegOpenKeyExA(
        HKEY_LOCAL_MACHINE,
        "SOFTWARE\\Microsoft\\Windows Kits\\Installed Roots".as_ptr() as _,
        0,
        KEY_QUERY_VALUE | KEY_WOW64_32KEY | KEY_ENUMERATE_SUB_KEYS,
        &mut main_key.inner,
    );

    if rc != S_OK {
        return;
    }

    // Look for a Windows 10 entry.
    if let Some(windows10_root) = read_from_the_registry(main_key.inner, u16cstr!("KitsRoot10").as_ptr()) {
        let windows10_root = windows10_root.as_ref() as *const u16;

        let mut data = VersionData {
            best_version: [0, 0, 0, 0],
            best_name: U16String::new(),
        };

        let mut windows10_lib = U16String::from_ptr(windows10_root, wcslen(windows10_root));
        windows10_lib.push(u16str!("Lib"));

        visit_files_w(windows10_lib, &mut data, win10_best);

        result.windows_sdk_version = 10;
        result.windows_sdk_root = Some(data.best_name);
        return;
    }
    // Look for a Windows 8 entry.
    else if let Some(windows8_root) = read_from_the_registry(main_key.inner, u16cstr!("KitsRoot81").as_ptr()) {
        let windows8_root = windows8_root.as_ref() as *const u16;

        let mut data = VersionData {
            best_version: [0, 0, 0, 0],
            best_name: U16String::new(),
        };

        let mut windows8_lib = U16String::from_ptr(windows8_root, wcslen(windows8_root));
        windows8_lib.push(u16str!("Lib"));

        visit_files_w(windows8_lib, &mut data, win8_best);

        result.windows_sdk_version = 8;
        result.windows_sdk_root = Some(data.best_name);
    }

    // If we get here, we failed to find anything.
}

unsafe fn find_visual_studio_2017_by_fighting_through_microsoft_craziness(result: &mut FindResult) -> bool {
    // The name of this procedure is kind of cryptic. Its purpose is
    // to fight through Microsoft craziness. The things that the fine
    // Visual Studio team want you to do, JUST TO FIND A SINGLE FOLDER
    // THAT EVERYONE NEEDS TO FIND, are ridiculous garbage.

    // For earlier versions of Visual Studio, you'd find this information in the registry,
    // similarly to the Windows Kits above. But no, now it's the future, so to ask the
    // question "Where is the Visual Studio folder?" you have to do a bunch of COM object
    // instantiation, enumeration, and querying. (For extra bonus points, try doing this in
    // a new, underdeveloped programming language where you don't have COM routines up
    // and running yet. So fun.)
    //
    // If all this COM object instantiation, enumeration, and querying doesn't give us
    // a useful result, we drop back to the registry-checking method.

    let rc = CoInitializeEx(ptr::null_mut(), COINIT_MULTITHREADED);

    if rc != S_OK {
        return false;
    }

    const MY_UID: GUID = GUID {
        Data1: 0x42843719,
        Data2: 0xDB4C,
        Data3: 0x46C2,
        Data4: [0x8E, 0x7C, 0x64, 0xF1, 0x81, 0x6E, 0xFD, 0x5B],
    };

    const CLSID_SETUP_CONFIGURATION: GUID = GUID {
        Data1: 0x177F0C4A,
        Data2: 0x1CD3,
        Data3: 0x4DE7,
        Data4: [0xA3, 0x2C, 0x71, 0xDB, 0xBB, 0x9F, 0xA3, 0x6D],
    };

    let mut config = ptr::null_mut::<ISetupConfiguration>();
    let hr = CoCreateInstance(
        &CLSID_SETUP_CONFIGURATION,
        ptr::null_mut(),
        CLSCTX_INPROC_SERVER,
        &MY_UID,
        &mut config as *mut *mut ISetupConfiguration as _,
    );

    if hr != S_OK {
        return false;
    }

    let _d0 = defer(|| (*config).Release());

    let mut instances = ptr::null_mut::<IEnumSetupInstances>();
    let hr = (*config).EnumInstances(&mut instances);

    if hr != S_OK || instances.is_null() {
        return false;
    }

    let _d1 = defer(|| (*instances).Release());

    loop {
        let mut found: ULONG = 0;
        let mut instance = ptr::null_mut::<ISetupInstance>();

        let hr = (*instances).Next(1, &mut instance, &mut found);

        if hr != S_OK {
            break;
        }

        let _d2 = defer(|| (*instance).Release());

        let mut bstr_inst_path: BSTR = ptr::null_mut();
        let hr = (*instance).GetInstallationPath(&mut bstr_inst_path);

        if hr != S_OK {
            continue;
        }

        let _d3 = defer(|| SysFreeString(bstr_inst_path));

        let inst_path = U16String::from_ptr(bstr_inst_path, wcslen(bstr_inst_path));
        let mut tools_filename = inst_path.clone();
        tools_filename.push(u16str!("\\VC\\Auxiliary\\Build\\Microsoft.VCToolsVersion.default.txt"));

        match fs::read_to_string(tools_filename.to_string().unwrap()) {
            Ok(mut version) => {
                if version.ends_with('\n') {
                    version.pop();
                }

                if version.ends_with('\r') {
                    version.pop();
                }

                let mut library_path = inst_path.clone();
                library_path.push(u16str!("\\VC\\Tools\\MSVC\\"));
                library_path.push(U16String::from_str(&version));
                library_path.push(u16str!("\\lib\\x64"));

                let mut library_file = library_path.clone();
                library_file.push(u16str!("\\vcruntime.lib"));

                if os_file_exists(U16CString::from_ustr(library_file).unwrap().as_ptr()) {
                    let mut link_exe_path = inst_path.clone();
                    link_exe_path.push(u16str!("\\VC\\Tools\\MSVC\\"));
                    link_exe_path.push(U16String::from_str(&version));
                    link_exe_path.push(u16str!("\\bin\\Hostx64\\x64"));

                    result.vs_exe_path = Some(link_exe_path);
                    result.vs_library_path = Some(library_path);

                    return true;
                }
            }
            Err(_) => continue,
        }

        /*
           Ryan Saunderson said:
           "Clang uses the 'SetupInstance->GetInstallationVersion' / ISetupHelper->ParseVersion to find the newest version
           and then reads the tools file to define the tools path - which is definitely better than what i did."

           So... @Incomplete: Should probably pick the newest version...
        */
    }

    false
}

unsafe fn find_visual_studio_by_fighting_through_microsoft_craziness(result: &mut FindResult) {
    if find_visual_studio_2017_by_fighting_through_microsoft_craziness(result) {
        return;
    }

    let mut vs7_key = SafeHKey::new();
    let rc = RegOpenKeyExA(
        HKEY_LOCAL_MACHINE,
        "SOFTWARE\\Microsoft\\VisualStudio\\SxS\\VS7".as_ptr() as _,
        0,
        KEY_QUERY_VALUE | KEY_WOW64_32KEY,
        &mut vs7_key.inner,
    );

    if rc != S_OK {
        return;
    }

    // Hardcoded search for 4 prior Visual Studio versions. Is there something better to do here?
    let versions = [u16cstr!("14.0"), u16cstr!("12.0"), u16cstr!("11.0"), u16cstr!("10.0")];

    for version in versions {
        if let Some(buffer) = read_from_the_registry(vs7_key.inner, version.as_ptr()) {
            let root = buffer.as_ref() as *const u16;
            let root_path = U16String::from_ptr(root, wcslen(root));

            let mut lib_path = root_path.clone();
            lib_path.push(u16str!("VC\\Lib\\amd64"));

            // Check to see whether a vcruntime.lib actually exists here.
            let mut vcruntime_filename = lib_path.clone();
            vcruntime_filename.push(u16str!("\\vcruntime.lib"));

            if os_file_exists(vcruntime_filename.as_ptr()) {
                let mut vs_exe_path = root_path.clone();
                vs_exe_path.push(u16str!("VC\\bin"));

                result.vs_exe_path = Some(vs_exe_path);
                result.vs_library_path = Some(lib_path);
            }
        }
    }

    // If we get here, we failed to find anything.
}

pub unsafe fn find_visual_studio_and_windows_sdk() -> FindResult {
    let mut result = FindResult {
        windows_sdk_version: 0,
        windows_sdk_root: None,
        windows_sdk_um_library_path: None,
        windows_sdk_ucrt_library_path: None,
        vs_exe_path: None,
        vs_library_path: None,
    };

    find_windows_kit_root(&mut result);

    if let Some(windows_sdk_root) = &result.windows_sdk_root {
        let mut windows_sdk_um_library_path = windows_sdk_root.clone();
        windows_sdk_um_library_path.push(u16str!("\\um\\x64"));
        result.windows_sdk_um_library_path = Some(windows_sdk_um_library_path);

        let mut windows_sdk_ucrt_library_path = windows_sdk_root.clone();
        windows_sdk_ucrt_library_path.push(u16str!("\\ucrt\\x64"));
        result.windows_sdk_ucrt_library_path = Some(windows_sdk_ucrt_library_path);
    }

    find_visual_studio_by_fighting_through_microsoft_craziness(&mut result);

    result
}
