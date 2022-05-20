pub fn bit_width_to_size(w: u32) -> usize {
    ((w + 7) / 8) as usize
}

pub fn calculate_align(size: usize, align: usize) -> usize {
    if align > 0 {
        let res = size + align - 1;
        res - res % align
    } else {
        size
    }
}

pub fn calculate_align_from_offset(offset: usize, align: usize) -> usize {
    (offset + align - 1) / align * align
}
