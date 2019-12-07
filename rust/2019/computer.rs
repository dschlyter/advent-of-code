pub fn run_program(reg: &mut Vec<i32>, input: &Vec<i32>) -> Vec<i32> {
    let mut ip = 0;
    let mut input_pointer = 0;
    let mut output: Vec<i32> = [].to_vec();

    loop {
        let op_reg = reg[ip];
        let op = op_reg % 100;

        // dbg!("Executing", op);

        if op == 1 {
            let p = params(reg, ip, 2);
            let res_pos = reg[(ip+3) as usize] as usize;
            reg[res_pos] = p[0] + p[1];
            ip += 4;
        } else if op == 2 {
            let p = params(reg, ip, 2);
            let res_pos = reg[(ip+3) as usize] as usize;
            reg[res_pos] = p[0] * p[1];
            ip += 4;
        } else if op == 3 {
            let res_pos = reg[(ip+1) as usize] as usize;
            reg[res_pos] = input[input_pointer];
            input_pointer += 1;
            ip += 2;
        } else if op == 4 {
            let p = params(reg, ip, 1);
            output.push(p[0]);
            ip += 2;
        } else if op == 5 {
            let p = params(reg, ip, 2);
            if p[0] != 0 {
                ip = p[1] as usize;
            } else {
                ip += 3;
            }
        } else if op == 6 {
            let p = params(reg, ip, 2);
            if p[0] == 0 {
                ip = p[1] as usize;
            } else {
                ip += 3;
            }
        } else if op == 7 {
            let p = params(reg, ip, 2);
            let res_pos = reg[(ip+3) as usize] as usize;
            if p[0] < p[1] {
                reg[res_pos] = 1;
            } else {
                reg[res_pos] = 0;
            }
            ip += 4;
        } else if op == 8 {
            let p = params(reg, ip, 2);
            let res_pos = reg[(ip+3) as usize] as usize;
            if p[0] == p[1] {
                reg[res_pos] = 1;
            } else {
                reg[res_pos] = 0;
            }
            ip += 4;
        } else if op == 99 {
            break;
        } else {
            panic!(format!("Unsupported opcode! {}", op));
        }
    }

    return output;
}

fn params(reg: &Vec<i32>, ip: usize, param_count: usize) -> Vec<i32> {
    return (0..param_count)
        .map(|position| fetch(&reg, reg[ip], position as u32, reg[ip+1+position]))
        .collect();
}

fn fetch(reg: &Vec<i32>, op: i32, pos: u32, param: i32) -> i32 {
    let parmode = digit(op, pos + 2);

    if parmode == 0 {
        return reg[param as usize];
    } else if parmode == 1 {
        return param;
    } else {
        panic!("Unsupported parmod")
    }
}

fn digit(number: i32, pos: u32) -> i32 {
    let base: i32 = 10;
    return ((number as i32) % base.pow(pos+1)) / (base.pow(pos) as i32);
}