pub struct Computer {
    reg: Vec<i64>,
    input: Vec<i64>,
    output: Vec<i64>,
    ip: usize,
    input_pointer: usize,
    output_pointer: usize,
    relative_base: usize,
    terminated: bool
}

#[derive(PartialEq)]
pub enum RunMode {
    Finish,
    NextOutput
}

impl Computer {
    pub fn new(reg: &Vec<i64>) -> Computer {
        return Computer {
            reg: reg.clone(),
            input: [].to_vec(),
            output: [].to_vec(),
            ip: 0,
            input_pointer: 0,
            output_pointer: 0,
            terminated: false,
            relative_base: 0
        }
    }

    pub fn write(&mut self, data: i64) {
        self.input.push(data);
    }

    pub fn read(&mut self) -> i64 {
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
    pub fn process(&mut self, input: i64) {
        self.write(input);
        self.run_program(RunMode::NextOutput);
    }

    fn run_program(&mut self, run_mode: RunMode) {
        // TODO totally a hack, static allocation

        self.grow_memory(100_000);

        loop {
            let op_reg = self.reg[self.ip];
            let op = op_reg % 100;

            // println!("Executing {} {} {} {}", op_reg, self.reg[self.ip+1], self.reg[self.ip+2], self.reg[self.ip+3]);

            if op == 1 {
                let p = self.params(2);
                let res_pos = self.fetch_ret_pos(2);
                self.reg[res_pos] = p[0] + p[1];
                self.ip += 4;
            } else if op == 2 {
                let p = self.params(2);
                let res_pos = self.fetch_ret_pos(2);
                self.reg[res_pos] = p[0] * p[1];
                self.ip += 4;
            } else if op == 3 {
                let res_pos = self.fetch_ret_pos(0);
                self.reg[res_pos] = self.input[self.input_pointer];
                self.input_pointer += 1;
                self.ip += 2;
            } else if op == 4 {
                let p = self.params(1);
                self.output.push(p[0]);
                dbg!(format!("Output {}", p[0]));
                self.ip += 2;
                if run_mode == RunMode::NextOutput {
                    return;
                }
            } else if op == 5 {
                let p = self.params(2);
                if p[0] != 0 {
                    self.ip = p[1] as usize;
                } else {
                    self.ip += 3;
                }
            } else if op == 6 {
                let p = self.params(2);
                if p[0] == 0 {
                    self.ip = p[1] as usize;
                } else {
                    self.ip += 3;
                }
            } else if op == 7 {
                let p = self.params(2);
                let res_pos = self.fetch_ret_pos(2);
                if p[0] < p[1] {
                    self.reg[res_pos] = 1;
                } else {
                    self.reg[res_pos] = 0;
                }
                self.ip += 4;
            } else if op == 8 {
                let p = self.params(2);
                let res_pos = self.fetch_ret_pos(2);
                if p[0] == p[1] {
                    self.reg[res_pos] = 1;
                } else {
                    self.reg[res_pos] = 0;
                }
                self.ip += 4;
            } else if op == 9 {
                let p = self.params(1);
                self.relative_base = (self.relative_base as i64 + p[0]) as usize;
                self.ip += 2;
            } else if op == 99 {
                self.terminated = true;
                return;
            } else {
                panic!(format!("Unsupported opcode! {}", op));
            }
        }
    }

    fn params(&mut self, param_count: usize) -> Vec<i64> {
        let ip = self.ip;
        let params = (0..param_count)
            .map(|position| self.fetch(position as u32))
            .collect();
        // dbg!(&params);
        params
    }

    fn fetch(&mut self, pos: u32) -> i64 {
        let op = self.reg[self.ip];
        let parmode = self.digit(op, pos + 2);
        let param = self.reg[self.ip + 1 + (pos as usize)];

        if parmode == 0 {
            return self.reg[param as usize];
        } else if parmode == 1 {
            return param;
        } else if parmode == 2 {
            let pos = (self.relative_base as i64) + param;
            if pos < 0 {
                panic!("Relative base out of bounds");
            }
            return self.reg[pos as usize];
        } else {
            panic!("Unsupported parmod")
        }
    }

    fn fetch_ret_pos(&mut self, pos: u32) -> usize {
        let op = self.reg[self.ip];
        let parmode = self.digit(op, pos + 2);
        let param = self.reg[self.ip + 1 + (pos as usize)];

        if parmode == 0 {
            return param as usize;
        } else if parmode == 1 {
            panic!("parmode 1 for ret_pos makes no sense");
        } else if parmode == 2 {
            let pos = (self.relative_base as i64) + param;
            if pos < 0 {
                panic!("Relative base out of bounds");
            }
            return pos as usize;
        } else {
            panic!("Unsupported parmod")
        }
    }

    fn digit(&self, number: i64, pos: u32) -> i64 {
        let base: i64 = 10;
        return ((number as i64) % base.pow(pos+1)) / (base.pow(pos) as i64);
    }

    fn grow_memory(&mut self, size: usize) {
        while self.reg.len() <= size {
            self.reg.push(0);
        }
    }
}
