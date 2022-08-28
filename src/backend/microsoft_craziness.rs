use std::ptr;

use libc::{free, malloc, memcpy, wcslen};
use widestring::{u16cstr, u16str, U16CStr, U16String};
use winapi::{
    ctypes::wchar_t,
    shared::{
        minwindef::{DWORD, HKEY, HKEY__, LPFILETIME},
        ntdef::LPCWSTR,
        winerror::{ERROR_MORE_DATA, S_OK},
        wtypes::BSTR,
        wtypesbase::{LPCOLESTR, ULONG},
    },
    um::{
        fileapi::{FindClose, FindFirstFileW, FindNextFileW, GetFileAttributesW, INVALID_FILE_ATTRIBUTES},
        handleapi::INVALID_HANDLE_VALUE,
        minwinbase::WIN32_FIND_DATAW,
        winnt::{FILE_ATTRIBUTE_DIRECTORY, KEY_ENUMERATE_SUB_KEYS, KEY_QUERY_VALUE, KEY_WOW64_32KEY, REG_SZ},
        winreg::{RegCloseKey, RegOpenKeyExA, RegQueryValueExW, HKEY_LOCAL_MACHINE},
    },
};

#[derive(Debug, Clone)]
pub struct FindResult {
    pub windows_sdk_version: i32, // Zero if no Windows SDK found.
    pub windows_sdk_root: Option<U16String>,
    pub windows_sdk_um_library_path: Option<U16String>,
    pub windows_sdk_ucrt_library_path: Option<U16String>,
    pub vs_exe_path: Option<U16String>,
    pub vs_library_path: Option<U16String>,
}

#[allow(non_snake_case)]
#[repr(C)]
struct ISetupInstance {
    GetInstanceId: fn(*mut BSTR),
    GetInstallDate: fn(LPFILETIME),
    GetInstallationName: fn(*mut BSTR),
    GetInstallationPath: fn(*mut BSTR),
    GetInstallationVersion: fn(*mut BSTR),
    GetDisplayName: fn(*mut BSTR),
    GetDescription: fn(*mut BSTR),
    ResolvePath: fn(LPCOLESTR, *mut BSTR),
}

#[allow(non_snake_case)]
#[repr(C)]
struct IEnumSetupInstances {
    Next: fn(ULONG, *mut *mut ISetupInstance, *mut ULONG),
    Skip: fn(ULONG),
    Reset: fn(),
    Clone: fn(*mut *mut IEnumSetupInstances),
}

#[allow(non_snake_case)]
#[repr(C)]
struct ISetupConfiguration {
    EnumInstances: fn(*mut *mut IEnumSetupInstances),
    GetInstanceForCurrentProcess: fn(*mut *mut ISetupInstance),
    GetInstanceForPath: fn(LPCWSTR, *mut *mut ISetupInstance),
}

struct VersionData {
    best_version: [i32; 4],
    best_name: U16String,
}

fn os_file_exists(name: *const wchar_t) -> bool {
    let attr = unsafe { GetFileAttributesW(name) };
    attr != INVALID_FILE_ATTRIBUTES && attr & FILE_ATTRIBUTE_DIRECTORY == 0
}

unsafe fn concat([a, b, c, d]: [*const wchar_t; 4]) -> Box<wchar_t> {
    let len_a = wcslen(a);
    let len_b = wcslen(b);

    let len_c = if !c.is_null() { wcslen(c) } else { 0 };
    let len_d = if !d.is_null() { wcslen(d) } else { 0 };

    let result = malloc((len_a + len_b + len_c + len_d + 1) * 2) as *mut wchar_t;
    memcpy(result as _, a as _, len_a * 2);
    memcpy(result.add(len_a) as _, b as _, len_b * 2);

    if !c.is_null() {
        memcpy(result.add(len_a + len_b) as _, c as _, len_c * 2);
    }

    if !d.is_null() {
        memcpy(result.add(len_a + len_b + len_c) as _, d as _, len_d * 2);
    }

    *result.add(len_a + len_b + len_c + len_d) = 0;

    Box::from_raw(result)
}

unsafe fn concat2(a: *const wchar_t, b: *const wchar_t) -> Box<wchar_t> {
    concat([a, b, ptr::null_mut(), ptr::null_mut()])
}

unsafe fn concat3(a: *const wchar_t, b: *const wchar_t, c: *const wchar_t) -> Box<wchar_t> {
    concat([a, b, c, ptr::null_mut()])
}

unsafe fn concat4(a: *const wchar_t, b: *const wchar_t, c: *const wchar_t, d: *const wchar_t) -> Box<wchar_t> {
    concat([a, b, c, d])
}

type VisitFn = unsafe fn(*const wchar_t, *const wchar_t, &mut VersionData);

unsafe fn visit_files_w(dir_name: *mut wchar_t, data: &mut VersionData, f: VisitFn) -> bool {
    let wildcard_name = concat2(dir_name, u16cstr!("\\*").as_ptr());

    let mut find_data = WIN32_FIND_DATAW::default();

    let handle = FindFirstFileW(wildcard_name.as_ref() as _, &mut find_data);

    let dot = u16str!(".").as_slice()[0];

    if handle != INVALID_HANDLE_VALUE {
        loop {
            if (find_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY != 0) && (find_data.cFileName[0] != dot) {
                let mut full_name = concat3(dir_name, u16cstr!("\\").as_ptr(), find_data.cFileName.as_ptr());
                f(find_data.cFileName.as_ptr(), full_name.as_mut() as _, data);
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

            if rc != 0 || type_ != REG_SZ {
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

unsafe fn win10_best(short_name: *const wchar_t, full_name: *const wchar_t, data: &mut VersionData) {
    let string = U16CStr::from_ptr(short_name, wcslen(short_name))
        .unwrap()
        .to_string()
        .unwrap();

    let parts = string
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
        data.best_name = U16String::from_ptr(full_name, wcslen(full_name));

        data.best_version[0] = i0;
        data.best_version[1] = i1;
        data.best_version[2] = i2;
        data.best_version[3] = i3;
    }
}

unsafe fn win8_best(short_name: *const wchar_t, full_name: *const wchar_t, data: &mut VersionData) {
    let string = U16CStr::from_ptr(short_name, wcslen(short_name))
        .unwrap()
        .to_string()
        .unwrap();

    let parts = string
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
        data.best_name = U16String::from_ptr(full_name, wcslen(full_name));

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
        let mut data = VersionData {
            best_version: [0, 0, 0, 0],
            best_name: U16String::new(),
        };

        let mut windows10_lib = concat2(windows10_root.as_ref() as _, u16cstr!("Lib").as_ptr());

        visit_files_w(windows10_lib.as_mut() as _, &mut data, win10_best);

        result.windows_sdk_version = 10;
        result.windows_sdk_root = Some(data.best_name);
        return;
    }
    // Look for a Windows 8 entry.
    else if let Some(windows8_root) = read_from_the_registry(main_key.inner, u16cstr!("KitsRoot81").as_ptr()) {
        let mut data = VersionData {
            best_version: [0, 0, 0, 0],
            best_name: U16String::new(),
        };

        let mut windows8_lib = concat2(windows8_root.as_ref() as _, u16cstr!("Lib").as_ptr());

        visit_files_w(windows8_lib.as_mut() as _, &mut data, win8_best);

        result.windows_sdk_version = 8;
        result.windows_sdk_root = Some(data.best_name);
    }

    // If we get here, we failed to find anything.
}

// bool find_visual_studio_2017_by_fighting_through_microsoft_craziness(Find_Result *result) {
//     // The name of this procedure is kind of cryptic. Its purpose is
//     // to fight through Microsoft craziness. The things that the fine
//     // Visual Studio team want you to do, JUST TO FIND A SINGLE FOLDER
//     // THAT EVERYONE NEEDS TO FIND, are ridiculous garbage.

//     // For earlier versions of Visual Studio, you'd find this information in the registry,
//     // similarly to the Windows Kits above. But no, now it's the future, so to ask the
//     // question "Where is the Visual Studio folder?" you have to do a bunch of COM object
//     // instantiation, enumeration, and querying. (For extra bonus points, try doing this in
//     // a new, underdeveloped programming language where you don't have COM routines up
//     // and running yet. So fun.)
//     //
//     // If all this COM object instantiation, enumeration, and querying doesn't give us
//     // a useful result, we drop back to the registry-checking method.

//     auto rc = CoInitializeEx(NULL, COINIT_MULTITHREADED);
//     // "Subsequent valid calls return false." So ignore false.
//     // if rc != S_OK  return false;

//     GUID my_uid                   = {0x42843719, 0xDB4C, 0x46C2, {0x8E, 0x7C, 0x64, 0xF1, 0x81, 0x6E, 0xFD, 0x5B}};
//     GUID CLSID_SetupConfiguration = {0x177F0C4A, 0x1CD3, 0x4DE7, {0xA3, 0x2C, 0x71, 0xDB, 0xBB, 0x9F, 0xA3, 0x6D}};

//     ISetupConfiguration *config = NULL;
//     auto hr = CoCreateInstance(CLSID_SetupConfiguration, NULL, CLSCTX_INPROC_SERVER, my_uid, (void **)&config);
//     if (hr != 0)  return false;
//     defer { config->Release(); };

//     IEnumSetupInstances *instances = NULL;
//     hr = config->EnumInstances(&instances);
//     if (hr != 0)     return false;
//     if (!instances)  return false;
//     defer { instances->Release(); };

//     while (1) {
//         ULONG found = 0;
//         ISetupInstance *instance = NULL;
//         auto hr = instances->Next(1, &instance, &found);
//         if (hr != S_OK) break;

//         defer { instance->Release(); };

//         BSTR bstr_inst_path;
//         hr = instance->GetInstallationPath(&bstr_inst_path);
//         if (hr != S_OK)  continue;
//         defer { SysFreeString(bstr_inst_path); };

//         auto tools_filename = concat(bstr_inst_path, L"\\VC\\Auxiliary\\Build\\Microsoft.VCToolsVersion.default.txt");
//         defer { free(tools_filename); };

//         FILE *f = nullptr;
//         auto open_result = _wfopen_s(&f, tools_filename, L"rt");
//         if (open_result != 0) continue;
//         if (!f) continue;
//         defer { fclose(f); };

//         LARGE_INTEGER tools_file_size;
//         auto file_handle = (HANDLE)_get_osfhandle(_fileno(f));
//         BOOL success = GetFileSizeEx(file_handle, &tools_file_size);
//         if (!success) continue;

//         auto version_bytes = (tools_file_size.QuadPart + 1) * 2;  // Warning: This multiplication by 2 presumes there is no variable-length encoding in the wchars (wacky characters in the file could betray this expectation).
//         wchar_t *version = (wchar_t *)malloc(version_bytes);
//         defer { free(version); };

//         auto read_result = fgetws(version, version_bytes, f);
//         if (!read_result) continue;

//         auto version_tail = wcschr(version, '\n');
//         if (version_tail)  *version_tail = 0;  // Stomp the data, because nobody cares about it.

//         auto library_path = concat(bstr_inst_path, L"\\VC\\Tools\\MSVC\\", version, L"\\lib\\x64");
//         auto library_file = concat(library_path, L"\\vcruntime.lib");  // @Speed: Could have library_path point to this string, with a smaller count, to save on memory flailing!

//         if (os_file_exists(library_file)) {
//             auto link_exe_path = concat(bstr_inst_path, L"\\VC\\Tools\\MSVC\\", version, L"\\bin\\Hostx64\\x64");
//             result->vs_exe_path     = link_exe_path;
//             result->vs_library_path = library_path;
//             return true;
//         }

//         /*
//            Ryan Saunderson said:
//            "Clang uses the 'SetupInstance->GetInstallationVersion' / ISetupHelper->ParseVersion to find the newest version
//            and then reads the tools file to define the tools path - which is definitely better than what i did."

//            So... @Incomplete: Should probably pick the newest version...
//         */
//     }

//     // If we get here, we didn't find Visual Studio 2017. Try earlier versions.
//     return false;
// }

// void find_visual_studio_by_fighting_through_microsoft_craziness(Find_Result *result) {
//     bool found_visual_studio_2017 = find_visual_studio_2017_by_fighting_through_microsoft_craziness(result);
//     if (found_visual_studio_2017) return;

//     HKEY vs7_key;
//     auto rc = RegOpenKeyExA(HKEY_LOCAL_MACHINE, "SOFTWARE\\Microsoft\\VisualStudio\\SxS\\VS7", 0, KEY_QUERY_VALUE | KEY_WOW64_32KEY, &vs7_key);

//     if (rc != S_OK)  return;
//     defer { RegCloseKey(vs7_key); };

//     // Hardcoded search for 4 prior Visual Studio versions. Is there something better to do here?
//     wchar_t *versions[] = { L"14.0", L"12.0", L"11.0", L"10.0" };
//     const int NUM_VERSIONS = sizeof(versions) / sizeof(versions[0]);

//     for (int i = 0; i < NUM_VERSIONS; i++) {
//         auto v = versions[i];

//         auto buffer = read_from_the_registry(vs7_key, v);
//         if (!buffer) continue;

//         defer { free(buffer); };

//         auto lib_path = concat(buffer, L"VC\\Lib\\amd64");

//         // Check to see whether a vcruntime.lib actually exists here.
//         auto vcruntime_filename = concat(lib_path, L"\\vcruntime.lib");
//         defer { free(vcruntime_filename); };

//         if (os_file_exists(vcruntime_filename)) {
//             result->vs_exe_path     = concat(buffer, L"VC\\bin\\amd64");
//             result->vs_library_path = lib_path;
//             return;
//         }

//         free(lib_path);
//     }

//     // If we get here, we failed to find anything.
// }

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

    dbg!(&result);

    //     if (result.windows_sdk_root) {
    //         result.windows_sdk_um_library_path   = concat(result.windows_sdk_root, L"\\um\\x64");
    //         result.windows_sdk_ucrt_library_path = concat(result.windows_sdk_root, L"\\ucrt\\x64");
    //     }

    //     find_visual_studio_by_fighting_through_microsoft_craziness(&result);

    result
}
