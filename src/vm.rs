// See https://github.com/wasm3/wasm3/blob/main/docs/Interpreter.md

// According to wasm3, tail recursion is faster because
// the function call arguments are mapped to CPU registers.
struct Op(fn(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, f: f32));

fn read4(mem: &[u8], addr: usize) -> [u8; 4] {
    assert!(addr + 3 < mem.len());
    [mem[addr], mem[addr + 1], mem[addr + 2], mem[addr + 3]]
}

fn write4(mem: &mut [u8], addr: usize, word: [u8; 4]) {
    assert!(addr + 3 < mem.len());
    mem[addr] = word[0];
    mem[addr + 1] = word[1];
    mem[addr + 2] = word[2];
    mem[addr + 3] = word[3];
}

fn sp_up(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, f: f32) {
    (code[ip + 1].0)(code, imm, ip + 1, mem, sp+4, i, f);
}

fn sp_down(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, f: f32) {
    (code[ip + 1].0)(code, imm, ip + 1, mem, sp-4, i, f);
}

fn i_add(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, f: f32) {
    let x = i32::from_ne_bytes(read4(mem, sp));
    (code[ip + 1].0)(code, imm, ip + 1, mem, sp, i + x, f);
}

fn i_sub(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, f: f32) {
    let x = i32::from_ne_bytes(read4(mem, sp));
    (code[ip + 1].0)(code, imm, ip + 1, mem, sp, i - x, f);
}

fn f_add(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, f: f32) {
    let x = f32::from_ne_bytes(read4(mem, sp));
    (code[ip + 1].0)(code, imm, ip + 1, mem, sp, i, f + x);
}

fn f_sub(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, f: f32) {
    let x = f32::from_ne_bytes(read4(mem, sp));
    (code[ip + 1].0)(code, imm, ip + 1, mem, sp, i, f - x);
}

fn f_mul(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, f: f32) {
    let x = f32::from_ne_bytes(read4(mem, sp));
    (code[ip + 1].0)(code, imm, ip + 1, mem, sp, i, f * x);
}

fn f_div(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, f: f32) {
    let x = f32::from_ne_bytes(read4(mem, sp));
    (code[ip + 1].0)(code, imm, ip + 1, mem, sp, i, f / x);
}

fn f_neg(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, f: f32) {
    (code[ip + 1].0)(code, imm, ip + 1, mem, sp, i, -f);
}

fn f_inv(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, f: f32) {
    (code[ip + 1].0)(code, imm, ip + 1, mem, sp, i, 1.0 / f);
}

fn f_sin(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, f: f32) {
    (code[ip + 1].0)(code, imm, ip + 1, mem, sp, i, f.sin());
}

fn f_cos(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, f: f32) {
    (code[ip + 1].0)(code, imm, ip + 1, mem, sp, i, f.cos());
}

/// Load value top of stack into fp register.
fn f_load(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, _f: f32) {
    let x = f32::from_ne_bytes(read4(mem, sp));
    (code[ip + 1].0)(code, imm, ip + 1, mem, sp, i, x);
}

/// Save fp register to top of stack.
fn f_store(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, f: f32) {
    write4(mem, sp, f.to_ne_bytes());
    (code[ip + 1].0)(code, imm, ip + 1, mem, sp, i, f);
}

/// Load immediate floating point value into f.
fn f_imm(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, _f: f32) {
    let f = f32::from_ne_bytes(read4(imm, ip * 4));
    (code[ip + 1].0)(code, imm, ip + 1, mem, sp, i, f);
}

/// Branch if zero.
fn i_bz(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, f: f32) {
    let ip = if i == 0 {
        u32::from_ne_bytes(read4(imm, ip * 4)) as usize
    } else {
        ip + 1
    };
    (code[ip].0)(code, imm, ip, mem, sp, i, f);
}

/// Branch if f is zero.
fn f_bz(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, f: f32) {
    let ip = if f == 0.0 {
        u32::from_ne_bytes(read4(imm, ip * 4)) as usize
    } else {
        ip + 1
    };
    (code[ip].0)(code, imm, ip, mem, sp, i, f);
}

/// Call a function. This uses our own call stack to store the registers.
fn call(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, f: f32) {
    let func = u32::from_ne_bytes(read4(imm, ip * 4)) as usize;
    (code[func].0)(code, imm, func, mem, sp, i, f);
    (code[ip+1].0)(code, imm, ip+1, mem, sp, i, f);
}

/// End of the code or end of a function. Terminates tail recursion.
fn end(_code: &[Op], _imm: &[u8], _ip: usize, _mem: &mut [u8], _sp: usize, _i: i32, _f: f32) { }

impl Op {
    fn name(&self) -> &'static str {
        let i = self.0 as usize;
        if i == end as usize { "end" } 
        else if i == f_imm as usize { "f_imm" }
        else if i == f_store as usize { "f_store" }
        else if i == call as usize { "call" }
        else { panic!() }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_imm_store() {
        let code = [Op(f_imm), Op(f_store), Op(end)];

        code.iter().for_each(|op| println!("{}", op.name()));

        let imm = (42.0 as f32).to_ne_bytes();
        let mut mem = [0 as u8; 4];

        assert_ne!(f32::from_ne_bytes(mem), 42.0);
        
        (code[0].0)(&code, &imm, 0, &mut mem, 0, 0, 0.0);

        assert_eq!(f32::from_ne_bytes(mem), 42.0);
    }

    #[test]
    fn test_call() {
        let code = [Op(call), Op(end), Op(f_imm), Op(f_store), Op(end)];

        code.iter().for_each(|op| println!("{}", op.name()));

        let imm = [(2 as i32).to_ne_bytes(), (0 as i32).to_ne_bytes(), (42.0 as f32).to_ne_bytes()].concat();
        let mut mem = [0 as u8; 4];
        
        (code[0].0)(&code, &imm, 0, &mut mem, 0, 0, 0.0);

        assert_eq!(f32::from_ne_bytes(mem), 42.0);
    }
}