pub fn read4(mem: &[u8], addr: usize) -> [u8; 4] {
    assert!(addr + 3 < mem.len());
    [mem[addr], mem[addr + 1], mem[addr + 2], mem[addr + 3]]
}

pub fn write4(mem: &mut [u8], addr: usize, word: [u8; 4]) {
    assert!(addr + 3 < mem.len());
    mem[addr] = word[0];
    mem[addr + 1] = word[1];
    mem[addr + 2] = word[2];
    mem[addr + 3] = word[3];
}

pub fn cmp(a: i32, b: i32) -> i32 {
    if a < b {
        1
    } else {
        0
    }
}
