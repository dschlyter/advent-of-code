pub struct Computer {
    reg: Vec<i32>,
    input: Vec<i32>,
    output: Vec<i32>,
    ip: usize,
    input_pointer: usize,
    output_pointer: usize,
    terminated: bool
}

#[derive(PartialEq)]
enum RunMode {
    Finish,
    NextOutput
}

impl Computer {
    pub fn new(reg: &Vec<i32>) -> Computer {
        return Computer {
            reg: reg.clone(),
            input: [].to_vec(),
            output: [].to_vec(),
            ip: 0,
            input_pointer: 0,
            output_pointer: 0,
            terminated: false
        }
    }

    pub fn write(&mut self, data: i32) {
        self.input.push(data);
    }

    pub fn read(&mut self) -> i32 {
        let ret = self.output[self.output_pointer];
        self.output_pointer += 1;
        return ret;
    }

    pub fn done(&self) -> bool {
        return self.terminated;
    }

    pub fn run(&mut self) {
        self.run_program(RunMode::Finish);
    }

    // Send one input and run until next output (or termination)
    pub fn process(&mut self, input: i32) {
        self.write(input);
        self.run_program(RunMode::NextOutput);
    }

    fn run_program(&mut self, run_mode: RunMode) {
        loop {
            let op_reg = self.reg[self.ip];
            let op = op_reg % 100;

            // dbg!("Executing", op);

            if op == 1 {
                let p = params(&self.reg, self.ip, 2);
                let res_pos = self.reg[(self.ip+3) as usize] as usize;
                self.reg[res_pos] = p[0] + p[1];
                self.ip += 4;
            } else if op == 2 {
                let p = params(&self.reg, self.ip, 2);
                let res_pos = self.reg[(self.ip+3) as usize] as usize;
                self.reg[res_pos] = p[0] * p[1];
                self.ip += 4;
            } else if op == 3 {
                let res_pos = self.reg[(self.ip+1) as usize] as usize;
                self.reg[res_pos] = self.input[self.input_pointer];
                self.input_pointer += 1;
                self.ip += 2;
            } else if op == 4 {
                let p = params(&self.reg, self.ip, 1);
                self.output.push(p[0]);
                self.ip += 2;
                if run_mode == RunMode::NextOutput {
                    return;
                }
            } else if op == 5 {
                let p = params(&self.reg, self.ip, 2);
                if p[0] != 0 {
                    self.ip = p[1] as usize;
                } else {
                    self.ip += 3;
                }
            } else if op == 6 {
                let p = params(&self.reg, self.ip, 2);
                if p[0] == 0 {
                    self.ip = p[1] as usize;
                } else {
                    self.ip += 3;
                }
            } else if op == 7 {
                let p = params(&self.reg, self.ip, 2);
                let res_pos = self.reg[(self.ip+3) as usize] as usize;
                if p[0] < p[1] {
                    self.reg[res_pos] = 1;
                } else {
                    self.reg[res_pos] = 0;
                }
                self.ip += 4;
            } else if op == 8 {
                let p = params(&self.reg, self.ip, 2);
                let res_pos = self.reg[(self.ip+3) as usize] as usize;
                if p[0] == p[1] {
                    self.reg[res_pos] = 1;
                } else {
                    self.reg[res_pos] = 0;
                }
                self.ip += 4;
            } else if op == 99 {
                self.terminated = true;
                return;
            } else {
                panic!(format!("Unsupported opcode! {}", op));
            }
        }
    }
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