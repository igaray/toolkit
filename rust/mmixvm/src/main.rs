use std::env;

mod mmix {

    /*
    #[derive(Clone, Copy, Eq, PartialEq, Debug)]
    struct Byte(u8);

    #[derive(Clone, Copy, Eq, PartialEq, Debug)]
    struct Wyde(u16);

    #[derive(Clone, Copy, Eq, PartialEq, Debug)]
    struct Tetra(u32);

    #[derive(Clone, Copy, Eq, PartialEq, Debug)]
    struct Octa(u64);
    */

    /// Special register codes used to index into the special register array.
    #[allow(non_snake_case)]
    #[allow(non_upper_case_globals)]
    mod SpecialRegister {
        /// Arithmetic status register
        /// - saved?: yes
        /// - put?: yes
        pub const rA: usize = 21;

        /// Bootstrap register (trip)
        /// - saved?: yes
        /// - put?: yes
        pub const rB: usize = 0;

        /// Continuation register, cycle counter
        /// - saved?: no
        /// - put?: no
        pub const rC: usize = 8;

        /// Dividend register
        /// Unsigned integer divide uses this as the left half of the 128-bit input that is to be divided by the other operand.
        /// - saved?:
        /// - put?:
        pub const rD: usize = 1;

        /// Epsilon register
        /// Used for floating comparisons with respect to epsilon.
        /// - saved?: yes
        /// - put?: yes
        pub const rE: usize = 2;

        /// Failure location register
        /// - saved?: yes
        /// - put?: no
        pub const rF: usize = 22;

        /// Global threshold register
        /// - saved?: yes
        /// - put?: yes
        pub const rG: usize = 19;

        /// Himult register
        /// Used to store the left half of the 128-bit result of unsigned integer multiplication.
        /// - saved?: yes
        /// - put?: yes
        pub const rH: usize = 3;

        /// Interval counter
        /// - saved?: no
        /// - put?: no
        pub const rI: usize = 12;

        /// Return-jump register
        /// - saved?: yes
        /// - put?: yes
        pub const rJ: usize = 4;

        /// Interrupt mask register
        /// - saved?: no
        /// - put?: no
        pub const rK: usize = 15;

        /// Local threshold register
        /// - saved?: yes
        /// - put?: yes
        pub const rL: usize = 20;

        /// Multiplex mask register
        /// - saved?: yes
        /// - put?: yes
        pub const rM: usize = 5;

        /// Serial number
        /// - saved?: no
        /// - put?: no
        pub const rN: usize = 9;

        /// Register stack offset
        /// - saved?: no
        /// - put?: no
        pub const rO: usize = 10;

        /// Prediction register
        /// - saved?: yes
        /// - put?: yes
        pub const rP: usize = 23;

        /// Interrupt request register
        /// - saved?: no
        /// - put?: no
        pub const rQ: usize = 16;

        /// Remainder register
        /// - saved?: yes
        /// - put?: yes
        pub const rR: usize = 6;

        /// Register stack pointer
        /// - saved?: no
        /// - put?: no
        pub const rS: usize = 11;

        /// Trap address register
        /// - saved?: no
        /// - put?: no
        pub const rT: usize = 13;

        /// Usage counter
        /// - saved?: no
        /// - put?: no
        pub const rU: usize = 17;

        /// Virtual translation register
        /// - saved?: no
        /// - put?: no
        pub const rV: usize = 18;

        /// Where-interrupted register
        /// - saved?: yes
        /// - put?: yes
        pub const rW: usize = 24;

        /// Execution register (trip)
        /// - saved?: yes
        /// - put?: yes
        pub const rX: usize = 25;

        /// Y operand (trip)
        /// - saved?: yes
        /// - put?: yes
        pub const rY: usize = 26;

        /// Z operand (trip)
        /// - saved?: yes
        /// - put?: yes
        pub const rZ: usize = 27;

        /// Bootstrap register (trap)
        /// - saved?: no
        /// - put?: yes
        pub const rBB: usize = 7;

        /// Dynamic trap address register
        /// - saved?: no
        /// - put?: no
        pub const rTT: usize = 14;

        /// Where-interrupted register (trap)
        /// - saved?: no
        /// - put?: yes
        pub const rWW: usize = 28;

        /// Execution register (trap)
        /// - saved?: no
        /// - put?: yes
        pub const rXX: usize = 29;

        /// Y operand (trap)
        /// - saved?: no
        /// - put?: yes
        pub const rYY: usize = 30;

        /// Z operand (trap)
        /// - saved?: no
        /// - put?: yes
        pub const rZZ: usize = 31;
    }

    #[repr(u8)]
    enum Opcode {
        TRAP = 0x00,
        FCMP = 0x01,
        FUN = 0x02,
        FEQL = 0x03,
        FADD = 0x04,
        FIX = 0x05,
        FSUB = 0x06,
        FIXU = 0x07,
        FLOT = 0x08,
        FLOTI = 0x09,
        FLOTU = 0x0A,
        FLOTUI = 0x0B,
        SFLOT = 0x0C,
        SFLOTI = 0x0D,
        SFLOTU = 0x0E,
        SFLOTUI = 0x0F,
        FMUL = 0x10,
        FCMPE = 0x11,
        FUNE = 0x12,
        FEQLE = 0x13,
        FDIV = 0x14,
        FSQRT = 0x15,
        FREM = 0x16,
        FINT = 0x17,
        MUL = 0x18,
        MULI = 0x19,
        MULU = 0x1A,
        MULUI = 0x1B,
        DIV = 0x1C,
        DIVI = 0x1D,
        DIVU = 0x1E,
        DIVUI = 0x1F,
        ADD = 0x20,
        ADDI = 0x21,
        ADDU = 0x22,
        ADDUI = 0x23,
        SUB = 0x24,
        SUBI = 0x25,
        SUBU = 0x26,
        SUBUI = 0x27,
        ADD2U = 0x28,
        ADD2UI = 0x29,
        ADD4U = 0x2A,
        ADD4UI = 0x2B,
        ADD8U = 0x2C,
        ADD8UI = 0x2D,
        ADD16U = 0x2E,
        ADD16UI = 0x2F,
        CMP = 0x30,
        CMPI = 0x31,
        CMPU = 0x32,
        CMPUI = 0x33,
        NEG = 0x34,
        NEGI = 0x35,
        NEGU = 0x36,
        NEGUI = 0x37,
        SL = 0x38,
        SLI = 0x39,
        SLU = 0x3A,
        SLUI = 0x3B,
        SR = 0x3C,
        SRI = 0x3D,
        SRU = 0x3E,
        SRUI = 0x3F,
        BN = 0x40,
        BNB = 0x41,
        BZ = 0x42,
        BZB = 0x43,
        BP = 0x44,
        BPB = 0x45,
        BOD = 0x46,
        BODB = 0x47,
        BNN = 0x48,
        BNNB = 0x49,
        BNZ = 0x4A,
        BNZB = 0x4B,
        BNP = 0x4C,
        BNPB = 0x4D,
        BEV = 0x4E,
        BEVB = 0x4F,
        PBN = 0x50,
        PBNB = 0x51,
        PBZ = 0x52,
        PBZB = 0x53,
        PBP = 0x54,
        PBPB = 0x55,
        PBOD = 0x56,
        PBODB = 0x57,
        PBNN = 0x58,
        PBNNB = 0x59,
        PBNZ = 0x5A,
        PBNZB = 0x5B,
        PBNP = 0x5C,
        PBNPB = 0x5D,
        PBEV = 0x5E,
        PBEVB = 0x5F,
        CSN = 0x60,
        CSNI = 0x61,
        CSZ = 0x62,
        CSZI = 0x63,
        CSP = 0x64,
        CSPI = 0x65,
        CSOD = 0x66,
        CSODI = 0x67,
        CSNN = 0x68,
        CSNNI = 0x69,
        CSNZ = 0x6A,
        CSNZI = 0x6B,
        CSNP = 0x6C,
        CSNPI = 0x6D,
        CSEV = 0x6E,
        CSEVI = 0x6F,
        ZSN = 0x70,
        ZSNI = 0x71,
        ZSZ = 0x72,
        ZSZI = 0x73,
        ZSP = 0x74,
        ZSPI = 0x75,
        ZSOD = 0x76,
        ZSODI = 0x77,
        ZSNN = 0x78,
        ZSNNI = 0x79,
        ZSNZ = 0x7A,
        ZSNZI = 0x7B,
        ZSNP = 0x7C,
        ZSNPI = 0x7D,
        ZSEV = 0x7E,
        ZSEVI = 0x7F,
        LDB = 0x80,
        LDBI = 0x81,
        LDBU = 0x82,
        LDBUI = 0x83,
        LDW = 0x84,
        LDWI = 0x85,
        LDWU = 0x86,
        LDWUI = 0x87,
        LDT = 0x88,
        LDTI = 0x89,
        LDTU = 0x8A,
        LDTUI = 0x8B,
        LDO = 0x8C,
        LDOI = 0x8D,
        LDOU = 0x8E,
        LDOUI = 0x8F,
        LDSF = 0x90,
        LDSFI = 0x91,
        LDHT = 0x92,
        LDHTI = 0x93,
        CSWAP = 0x94,
        CSWAPI = 0x95,
        LDUNC = 0x96,
        LDUNCI = 0x97,
        LDVTS = 0x98,
        LDVTSI = 0x99,
        PRELD = 0x9A,
        PRELDI = 0x9B,
        PREGO = 0x9C,
        PREGOI = 0x9D,
        GO = 0x9E,
        GOI = 0x9F,
        STB = 0xA0,
        STBI = 0xA1,
        STBU = 0xA2,
        STBUI = 0xA3,
        STW = 0xA4,
        STWI = 0xA5,
        STWU = 0xA6,
        STWUI = 0xA7,
        STT = 0xA8,
        STTI = 0xA9,
        STTU = 0xAA,
        STTUI = 0xAB,
        STO = 0xAC,
        STOI = 0xAD,
        STOU = 0xAE,
        STOUI = 0xAF,
        STSF = 0xB0,
        STSFI = 0xB1,
        STHT = 0xB2,
        STHTI = 0xB3,
        STCO = 0xB4,
        STCOII = 0xB5,
        STUNC = 0xB6,
        STUNCI = 0xB7,
        SYNCD = 0xB8,
        SYNCDI = 0xB9,
        PREST = 0xBA,
        PRESTI = 0xBB,
        SYNCID = 0xBC,
        SYNCIDI = 0xBD,
        PUSHGO = 0xBE,
        PUSHGOI = 0xBF,
        OR = 0xC0,
        ORI = 0xC1,
        ORN = 0xC2,
        ORNI = 0xC3,
        NOR = 0xC4,
        NORI = 0xC5,
        XOR = 0xC6,
        XORI = 0xC7,
        AND = 0xC8,
        ANDI = 0xC9,
        ANDN = 0xCA,
        ANDNI = 0xCB,
        NAND = 0xCC,
        NANDI = 0xCD,
        NXOR = 0xCE,
        NXORI = 0xCF,
        BDIF = 0xD0,
        BDIFI = 0xD1,
        WDIF = 0xD2,
        WDIFI = 0xD3,
        TDIF = 0xD4,
        TDIFI = 0xD5,
        ODIF = 0xD6,
        ODIFI = 0xD7,
        MUX = 0xD8,
        MUXI = 0xD9,
        SADD = 0xDA,
        SADDI = 0xDB,
        MOR = 0xDC,
        MORI = 0xDD,
        MXOR = 0xDE,
        MXORI = 0xDF,
        SETH = 0xE0,
        SETMH = 0xE1,
        SETML = 0xE2,
        SETL = 0xE3,
        INCH = 0xE4,
        INCMH = 0xE5,
        INCML = 0xE6,
        INCL = 0xE7,
        ORH = 0xE8,
        ORMH = 0xE9,
        ORML = 0xEA,
        ORL = 0xEB,
        ANDNH = 0xEC,
        ANDNMH = 0xED,
        ANDNML = 0xEE,
        ANDNL = 0xEF,
        JMP = 0xF0,
        JMPB = 0xF1,
        PUSHJ = 0xF2,
        PUSHJB = 0xF3,
        GETA = 0xF4,
        GETAB = 0xF5,
        PUT = 0xF6,
        PUTI = 0xF7,
        POP = 0xF8,
        RESUME = 0xF9,
        SAVE = 0xFA,
        UNSAVE = 0xFB,
        SYNC = 0xFC,
        SWYM = 0xFD,
        GET = 0xFE,
        TRIP = 0xFF,
    }

    /// An instruction or "command" is a tetrabyte whose four bytes are conventionally called OP, X, Y, and Z.
    /// OP is the operation code (or "opcode" for short); X, Y, and Z specify the operands.
    /// For example, #20010203 is an instruction with OP=#20, X=#01, Y=#02, and Z=#03, and it means "Set $1 to the sum of $2 and $3".
    /// The operand bytes are always regarded as unsigned integers.
    /// Most instructions have three operands, but some of them have only two, and a few have only one.
    /// When there are two operands, the first is X and the second is the two-byte quantity YZ.
    /// For example, the instruction 'INCL $X, YZ' increases register $X by the amount XY.
    /// When there is only one operand, it is the unsigned  
    pub enum Instruction {
        ThreeOperand { opcode: Opcode, x: u8, y: u8, z: u8 },
        TwoOperand { opcode: Opcode, x: u8, yz: [u8; 2] },
        SingleOperand { opcode: Opcode, xyz: [u8; 3] },
    }

    pub struct Program {
        instructions: Vec<Instruction>,
    }

    enum MemoryAccessBase {
        Two,
        Four,
        Eight,
    }

    enum MemoryValue {
        Byte(u8),
        Wyde(u16),
        Tetra(u32),
        Octa(u64),
    }

    ///
    pub struct Memory {
        contents: [u64; 1024],
    }

    impl Memory {
        fn new() -> Memory {
            return Memory {
                contents: [0u64; 1024],
            };
        }

        fn get(_base: MemoryAccessBase, _address: usize) -> MemoryValue {
            unimplemented!();
            MemoryValue::Byte(0)
        }

        fn set(_base: MemoryAccessBase, _address: usize, _value: MemoryValue) {
            unimplemented!();
        }
    }

    pub struct State {
        special: [u64; 32],
        general: [u64; 256],
        memory: Memory,
        running: bool,
    }

    impl State {
        pub fn new() -> State {
            State {
                special: [0u64; 32],
                general: [0u64; 256],
                memory: Memory::new(),
                running: false,
            }
        }
    }

    pub struct Config {
        pub debug: bool,
    }

    pub struct Mmixvm {
        config: Config,
        state: State,
        program: Option<Program>,
    }

    impl Mmixvm {
        pub fn new(config: Config) -> Mmixvm {
            Mmixvm {
                config: config,
                state: State::new(),
                program: None,
            }
        }

        pub fn load_program(&mut self, _program: &Program) {
            unimplemented!();
        }

        pub fn run(&mut self) -> Result<(), String> {
            self.state.running = true;
            while self.state.running {
                self.step()
            }
            Ok(())
        }

        pub fn step(&mut self) {
            // Fetch
            // Decode
            // Execute
            let opcode = Opcode::TRAP;
            match opcode {
                Opcode::TRAP => {
                    self.trap();
                }
                Opcode::FCMP => {
                    self.fcmp();
                }
                Opcode::FUN => {
                    self.fun();
                }
                Opcode::FEQL => {
                    self.feql();
                }
                Opcode::FADD => {
                    self.fadd();
                }
                Opcode::FIX => {
                    self.fix();
                }
                Opcode::FSUB => {
                    self.fsub();
                }
                Opcode::FIXU => {
                    self.fixu();
                }
                Opcode::FLOT => {
                    self.flot();
                }
                Opcode::FLOTI => {
                    self.floti();
                }
                Opcode::FLOTU => {
                    self.flotu();
                }
                Opcode::FLOTUI => {
                    self.flotui();
                }
                Opcode::SFLOT => {
                    self.sflot();
                }
                Opcode::SFLOTI => {
                    self.sfloti();
                }
                Opcode::SFLOTU => {
                    self.sflotu();
                }
                Opcode::SFLOTUI => {
                    self.sflotui();
                }
                Opcode::FMUL => {
                    self.fmul();
                }
                Opcode::FCMPE => {
                    self.fcmpe();
                }
                Opcode::FUNE => {
                    self.fune();
                }
                Opcode::FEQLE => {
                    self.feqle();
                }
                Opcode::FDIV => {
                    self.fdiv();
                }
                Opcode::FSQRT => {
                    self.fsqrt();
                }
                Opcode::FREM => {
                    self.frem();
                }
                Opcode::FINT => {
                    self.fint();
                }
                Opcode::MUL => {
                    self.mul();
                }
                Opcode::MULI => {
                    self.muli();
                }
                Opcode::MULU => {
                    self.mulu();
                }
                Opcode::MULUI => {
                    self.mului();
                }
                Opcode::DIV => {
                    self.div();
                }
                Opcode::DIVI => {
                    self.divi();
                }
                Opcode::DIVU => {
                    self.divu();
                }
                Opcode::DIVUI => {
                    self.divui();
                }
                Opcode::ADD => {
                    self.add();
                }
                Opcode::ADDI => {
                    self.addi();
                }
                Opcode::ADDU => {
                    self.addu();
                }
                Opcode::ADDUI => {
                    self.addui();
                }
                Opcode::SUB => {
                    self.sub();
                }
                Opcode::SUBI => {
                    self.subi();
                }
                Opcode::SUBU => {
                    self.subu();
                }
                Opcode::SUBUI => {
                    self.subui();
                }
                Opcode::ADD2U => {
                    self.add2u();
                }
                Opcode::ADD2UI => {
                    self.add2ui();
                }
                Opcode::ADD4U => {
                    self.add4u();
                }
                Opcode::ADD4UI => {
                    self.add4ui();
                }
                Opcode::ADD8U => {
                    self.add8u();
                }
                Opcode::ADD8UI => {
                    self.add8ui();
                }
                Opcode::ADD16U => {
                    self.add16u();
                }
                Opcode::ADD16UI => {
                    self.add16ui();
                }
                Opcode::CMP => {
                    self.cmp();
                }
                Opcode::CMPI => {
                    self.cmpi();
                }
                Opcode::CMPU => {
                    self.cmpu();
                }
                Opcode::CMPUI => {
                    self.cmpui();
                }
                Opcode::NEG => {
                    self.neg();
                }
                Opcode::NEGI => {
                    self.negi();
                }
                Opcode::NEGU => {
                    self.negu();
                }
                Opcode::NEGUI => {
                    self.negui();
                }
                Opcode::SL => {
                    self.sl();
                }
                Opcode::SLI => {
                    self.sli();
                }
                Opcode::SLU => {
                    self.slu();
                }
                Opcode::SLUI => {
                    self.slui();
                }
                Opcode::SR => {
                    self.sr();
                }
                Opcode::SRI => {
                    self.sri();
                }
                Opcode::SRU => {
                    self.sru();
                }
                Opcode::SRUI => {
                    self.srui();
                }
                Opcode::BN => {
                    self.bn();
                }
                Opcode::BNB => {
                    self.bnb();
                }
                Opcode::BZ => {
                    self.bz();
                }
                Opcode::BZB => {
                    self.bzb();
                }
                Opcode::BP => {
                    self.bp();
                }
                Opcode::BPB => {
                    self.bpb();
                }
                Opcode::BOD => {
                    self.bod();
                }
                Opcode::BODB => {
                    self.bodb();
                }
                Opcode::BNN => {
                    self.bnn();
                }
                Opcode::BNNB => {
                    self.bnnb();
                }
                Opcode::BNZ => {
                    self.bnz();
                }
                Opcode::BNZB => {
                    self.bnzb();
                }
                Opcode::BNP => {
                    self.bnp();
                }
                Opcode::BNPB => {
                    self.bnpb();
                }
                Opcode::BEV => {
                    self.bev();
                }
                Opcode::BEVB => {
                    self.bevb();
                }
                Opcode::PBN => {
                    self.pbn();
                }
                Opcode::PBNB => {
                    self.pbnb();
                }
                Opcode::PBZ => {
                    self.pbz();
                }
                Opcode::PBZB => {
                    self.pbzb();
                }
                Opcode::PBP => {
                    self.pbp();
                }
                Opcode::PBPB => {
                    self.pbpb();
                }
                Opcode::PBOD => {
                    self.pbod();
                }
                Opcode::PBODB => {
                    self.pbodb();
                }
                Opcode::PBNN => {
                    self.pbnn();
                }
                Opcode::PBNNB => {
                    self.pbnnb();
                }
                Opcode::PBNZ => {
                    self.pbnz();
                }
                Opcode::PBNZB => {
                    self.pbnzb();
                }
                Opcode::PBNP => {
                    self.pbnp();
                }
                Opcode::PBNPB => {
                    self.pbnpb();
                }
                Opcode::PBEV => {
                    self.pbev();
                }
                Opcode::PBEVB => {
                    self.pbevb();
                }
                Opcode::CSN => {
                    self.csn();
                }
                Opcode::CSNI => {
                    self.csni();
                }
                Opcode::CSZ => {
                    self.csz();
                }
                Opcode::CSZI => {
                    self.cszi();
                }
                Opcode::CSP => {
                    self.csp();
                }
                Opcode::CSPI => {
                    self.cspi();
                }
                Opcode::CSOD => {
                    self.csod();
                }
                Opcode::CSODI => {
                    self.csodi();
                }
                Opcode::CSNN => {
                    self.csnn();
                }
                Opcode::CSNNI => {
                    self.csnni();
                }
                Opcode::CSNZ => {
                    self.csnz();
                }
                Opcode::CSNZI => {
                    self.csnzi();
                }
                Opcode::CSNP => {
                    self.csnp();
                }
                Opcode::CSNPI => {
                    self.csnpi();
                }
                Opcode::CSEV => {
                    self.csev();
                }
                Opcode::CSEVI => {
                    self.csevi();
                }
                Opcode::ZSN => {
                    self.zsn();
                }
                Opcode::ZSNI => {
                    self.zsni();
                }
                Opcode::ZSZ => {
                    self.zsz();
                }
                Opcode::ZSZI => {
                    self.zszi();
                }
                Opcode::ZSP => {
                    self.zsp();
                }
                Opcode::ZSPI => {
                    self.zspi();
                }
                Opcode::ZSOD => {
                    self.zsod();
                }
                Opcode::ZSODI => {
                    self.zsodi();
                }
                Opcode::ZSNN => {
                    self.zsnn();
                }
                Opcode::ZSNNI => {
                    self.zsnni();
                }
                Opcode::ZSNZ => {
                    self.zsnz();
                }
                Opcode::ZSNZI => {
                    self.zsnzi();
                }
                Opcode::ZSNP => {
                    self.zsnp();
                }
                Opcode::ZSNPI => {
                    self.zsnpi();
                }
                Opcode::ZSEV => {
                    self.zsev();
                }
                Opcode::ZSEVI => {
                    self.zsevi();
                }
                Opcode::LDB => {
                    self.ldb();
                }
                Opcode::LDBI => {
                    self.ldbi();
                }
                Opcode::LDBU => {
                    self.ldbu();
                }
                Opcode::LDBUI => {
                    self.ldbui();
                }
                Opcode::LDW => {
                    self.ldw();
                }
                Opcode::LDWI => {
                    self.ldwi();
                }
                Opcode::LDWU => {
                    self.ldwu();
                }
                Opcode::LDWUI => {
                    self.ldwui();
                }
                Opcode::LDT => {
                    self.ldt();
                }
                Opcode::LDTI => {
                    self.ldti();
                }
                Opcode::LDTU => {
                    self.ldtu();
                }
                Opcode::LDTUI => {
                    self.ldtui();
                }
                Opcode::LDO => {
                    self.ldo();
                }
                Opcode::LDOI => {
                    self.ldoi();
                }
                Opcode::LDOU => {
                    self.ldou();
                }
                Opcode::LDOUI => {
                    self.ldoui();
                }
                Opcode::LDSF => {
                    self.ldsf();
                }
                Opcode::LDSFI => {
                    self.ldsfi();
                }
                Opcode::LDHT => {
                    self.ldht();
                }
                Opcode::LDHTI => {
                    self.ldhti();
                }
                Opcode::CSWAP => {
                    self.cswap();
                }
                Opcode::CSWAPI => {
                    self.cswapi();
                }
                Opcode::LDUNC => {
                    self.ldunc();
                }
                Opcode::LDUNCI => {
                    self.ldunci();
                }
                Opcode::LDVTS => {
                    self.ldvts();
                }
                Opcode::LDVTSI => {
                    self.ldvtsi();
                }
                Opcode::PRELD => {
                    self.preld();
                }
                Opcode::PRELDI => {
                    self.preldi();
                }
                Opcode::PREGO => {
                    self.prego();
                }
                Opcode::PREGOI => {
                    self.pregoi();
                }
                Opcode::GO => {
                    self.go();
                }
                Opcode::GOI => {
                    self.goi();
                }
                Opcode::STB => {
                    self.stb();
                }
                Opcode::STBI => {
                    self.stbi();
                }
                Opcode::STBU => {
                    self.stbu();
                }
                Opcode::STBUI => {
                    self.stbui();
                }
                Opcode::STW => {
                    self.stw();
                }
                Opcode::STWI => {
                    self.stwi();
                }
                Opcode::STWU => {
                    self.stwu();
                }
                Opcode::STWUI => {
                    self.stwui();
                }
                Opcode::STT => {
                    self.stt();
                }
                Opcode::STTI => {
                    self.stti();
                }
                Opcode::STTU => {
                    self.sttu();
                }
                Opcode::STTUI => {
                    self.sttui();
                }
                Opcode::STO => {
                    self.sto();
                }
                Opcode::STOI => {
                    self.stoi();
                }
                Opcode::STOU => {
                    self.stou();
                }
                Opcode::STOUI => {
                    self.stoui();
                }
                Opcode::STSF => {
                    self.stsf();
                }
                Opcode::STSFI => {
                    self.stsfi();
                }
                Opcode::STHT => {
                    self.stht();
                }
                Opcode::STHTI => {
                    self.sthti();
                }
                Opcode::STCO => {
                    self.stco();
                }
                Opcode::STCOII => {
                    self.stcoii();
                }
                Opcode::STUNC => {
                    self.stunc();
                }
                Opcode::STUNCI => {
                    self.stunci();
                }
                Opcode::SYNCD => {
                    self.syncd();
                }
                Opcode::SYNCDI => {
                    self.syncdi();
                }
                Opcode::PREST => {
                    self.prest();
                }
                Opcode::PRESTI => {
                    self.presti();
                }
                Opcode::SYNCID => {
                    self.syncid();
                }
                Opcode::SYNCIDI => {
                    self.syncidi();
                }
                Opcode::PUSHGO => {
                    self.pushgo();
                }
                Opcode::PUSHGOI => {
                    self.pushgoi();
                }
                Opcode::OR => {
                    self.or();
                }
                Opcode::ORI => {
                    self.ori();
                }
                Opcode::ORN => {
                    self.orn();
                }
                Opcode::ORNI => {
                    self.orni();
                }
                Opcode::NOR => {
                    self.nor();
                }
                Opcode::NORI => {
                    self.nori();
                }
                Opcode::XOR => {
                    self.xor();
                }
                Opcode::XORI => {
                    self.xori();
                }
                Opcode::AND => {
                    self.and();
                }
                Opcode::ANDI => {
                    self.andi();
                }
                Opcode::ANDN => {
                    self.andn();
                }
                Opcode::ANDNI => {
                    self.andni();
                }
                Opcode::NAND => {
                    self.nand();
                }
                Opcode::NANDI => {
                    self.nandi();
                }
                Opcode::NXOR => {
                    self.nxor();
                }
                Opcode::NXORI => {
                    self.nxori();
                }
                Opcode::BDIF => {
                    self.bdif();
                }
                Opcode::BDIFI => {
                    self.bdifi();
                }
                Opcode::WDIF => {
                    self.wdif();
                }
                Opcode::WDIFI => {
                    self.wdifi();
                }
                Opcode::TDIF => {
                    self.tdif();
                }
                Opcode::TDIFI => {
                    self.tdifi();
                }
                Opcode::ODIF => {
                    self.odif();
                }
                Opcode::ODIFI => {
                    self.odifi();
                }
                Opcode::MUX => {
                    self.mux();
                }
                Opcode::MUXI => {
                    self.muxi();
                }
                Opcode::SADD => {
                    self.sadd();
                }
                Opcode::SADDI => {
                    self.saddi();
                }
                Opcode::MOR => {
                    self.mor();
                }
                Opcode::MORI => {
                    self.mori();
                }
                Opcode::MXOR => {
                    self.mxor();
                }
                Opcode::MXORI => {
                    self.mxori();
                }
                Opcode::SETH => {
                    self.seth();
                }
                Opcode::SETMH => {
                    self.setmh();
                }
                Opcode::SETML => {
                    self.setml();
                }
                Opcode::SETL => {
                    self.setl();
                }
                Opcode::INCH => {
                    self.inch();
                }
                Opcode::INCMH => {
                    self.incmh();
                }
                Opcode::INCML => {
                    self.incml();
                }
                Opcode::INCL => {
                    self.incl();
                }
                Opcode::ORH => {
                    self.orh();
                }
                Opcode::ORMH => {
                    self.ormh();
                }
                Opcode::ORML => {
                    self.orml();
                }
                Opcode::ORL => {
                    self.orl();
                }
                Opcode::ANDNH => {
                    self.andnh();
                }
                Opcode::ANDNMH => {
                    self.andnmh();
                }
                Opcode::ANDNML => {
                    self.andnml();
                }
                Opcode::ANDNL => {
                    self.andnl();
                }
                Opcode::JMP => {
                    self.jmp();
                }
                Opcode::JMPB => {
                    self.jmpb();
                }
                Opcode::PUSHJ => {
                    self.pushj();
                }
                Opcode::PUSHJB => {
                    self.pushjb();
                }
                Opcode::GETA => {
                    self.geta();
                }
                Opcode::GETAB => {
                    self.getab();
                }
                Opcode::PUT => {
                    self.put();
                }
                Opcode::PUTI => {
                    self.puti();
                }
                Opcode::POP => {
                    self.pop();
                }
                Opcode::RESUME => {
                    self.resume();
                }
                Opcode::SAVE => {
                    self.save();
                }
                Opcode::UNSAVE => {
                    self.unsave();
                }
                Opcode::SYNC => {
                    self.sync();
                }
                Opcode::SWYM => {
                    self.swym();
                }
                Opcode::GET => {
                    self.get();
                }
                Opcode::TRIP => {
                    self.trip();
                }
            }
        }

        fn trap(&mut self) {
            if self.config.debug {
                println!("opcode: TRAP");
            }
            unimplemented!("opcode not implemented: TRAP");
        }

        fn fcmp(&mut self) {
            if self.config.debug {
                println!("opcode: FCMP");
            }
            unimplemented!("opcode not implemented: FCMP");
        }

        fn fun(&mut self) {
            if self.config.debug {
                println!("opcode: FUN");
            }
            unimplemented!("opcode not implemented: FUN");
        }

        fn feql(&mut self) {
            if self.config.debug {
                println!("opcode: FEQL");
            }
            unimplemented!("opcode not implemented: FEQL");
        }

        fn fadd(&mut self) {
            if self.config.debug {
                println!("opcode: FADD");
            }
            unimplemented!("opcode not implemented: FADD");
        }

        fn fix(&mut self) {
            if self.config.debug {
                println!("opcode: FIX");
            }
            unimplemented!("opcode not implemented: FIX");
        }

        fn fsub(&mut self) {
            if self.config.debug {
                println!("opcode: FSUB");
            }
            unimplemented!("opcode not implemented: FSUB");
        }

        fn fixu(&mut self) {
            if self.config.debug {
                println!("opcode: FIXU");
            }
            unimplemented!("opcode not implemented: FIXU");
        }

        fn flot(&mut self) {
            if self.config.debug {
                println!("opcode: FLOT");
            }
            unimplemented!("opcode not implemented: FLOT");
        }

        fn floti(&mut self) {
            if self.config.debug {
                println!("opcode: FLOTI");
            }
            unimplemented!("opcode not implemented: FLOTI");
        }

        fn flotu(&mut self) {
            if self.config.debug {
                println!("opcode: FLOTU");
            }
            unimplemented!("opcode not implemented: FLOTU");
        }

        fn flotui(&mut self) {
            if self.config.debug {
                println!("opcode: FLOTUI");
            }
            unimplemented!("opcode not implemented: FLOTUI");
        }

        fn sflot(&mut self) {
            if self.config.debug {
                println!("opcode: SFLOT");
            }
            unimplemented!("opcode not implemented: SFLOT");
        }

        fn sfloti(&mut self) {
            if self.config.debug {
                println!("opcode: SFLOTI");
            }
            unimplemented!("opcode not implemented: SFLOTI");
        }

        fn sflotu(&mut self) {
            if self.config.debug {
                println!("opcode: SFLOTU");
            }
            unimplemented!("opcode not implemented: SFLOTU");
        }

        fn sflotui(&mut self) {
            if self.config.debug {
                println!("opcode: SFLOTUI");
            }
            unimplemented!("opcode not implemented: SFLOTUI");
        }

        fn fmul(&mut self) {
            if self.config.debug {
                println!("opcode: FMUL");
            }
            unimplemented!("opcode not implemented: FMUL");
        }

        fn fcmpe(&mut self) {
            if self.config.debug {
                println!("opcode: FCMPE");
            }
            unimplemented!("opcode not implemented: FCMPE");
        }

        fn fune(&mut self) {
            if self.config.debug {
                println!("opcode: FUNE");
            }
            unimplemented!("opcode not implemented: FUNE");
        }

        fn feqle(&mut self) {
            if self.config.debug {
                println!("opcode: FEQLE");
            }
            unimplemented!("opcode not implemented: FEQLE")
        }

        fn fdiv(&mut self) {
            if self.config.debug {
                println!("opcode: FDIV");
            }
            unimplemented!("opcode not implemented: FDIV");
        }

        fn fsqrt(&mut self) {
            if self.config.debug {
                println!("opcode: FSQRT");
            }
            unimplemented!("opcode not implemented: FSQRT");
        }

        fn frem(&mut self) {
            if self.config.debug {
                println!("opcode: FREM");
            }
            unimplemented!("opcode not implemented: FREM");
        }

        fn fint(&mut self) {
            if self.config.debug {
                println!("opcode: FINT");
            }
            unimplemented!("opcode not implemented: FINT");
        }

        fn mul(&mut self) {
            if self.config.debug {
                println!("opcode: MUL");
            }
            unimplemented!("opcode not implemented: MUL");
        }

        fn muli(&mut self) {
            if self.config.debug {
                println!("opcode: MULI");
            }
            unimplemented!("opcode not implemented: MULI");
        }

        fn mulu(&mut self) {
            if self.config.debug {
                println!("opcode: MULU");
            }
            unimplemented!("opcode not implemented: MULU");
        }

        fn mului(&mut self) {
            if self.config.debug {
                println!("opcode: MULUI");
            }
            unimplemented!("opcode not implemented: MULUI");
        }

        fn div(&mut self) {
            if self.config.debug {
                println!("opcode: DIV");
            }
            unimplemented!("opcode not implemented: DIV");
        }

        fn divi(&mut self) {
            if self.config.debug {
                println!("opcode: DIVI");
            }
            unimplemented!("opcode not implemented: DIVI");
        }

        fn divu(&mut self) {
            if self.config.debug {
                println!("opcode: DIVU");
            }
            unimplemented!("opcode not implemented: DIVU");
        }

        fn divui(&mut self) {
            if self.config.debug {
                println!("opcode: DIVUI");
            }
            unimplemented!("opcode not implemented: DIVUI");
        }

        fn add(&mut self) {
            if self.config.debug {
                println!("opcode: ADD");
            }
            unimplemented!("opcode not implemented: ADD");
        }

        fn addi(&mut self) {
            if self.config.debug {
                println!("opcode: ADDI");
            }
            unimplemented!("opcode not implemented: ADDI");
        }

        fn addu(&mut self) {
            if self.config.debug {
                println!("opcode: ADDU");
            }
            unimplemented!("opcode not implemented: ADDU");
        }

        fn addui(&mut self) {
            if self.config.debug {
                println!("opcode: ADDUI");
            }
            unimplemented!("opcode not implemented: ADDUI");
        }

        fn sub(&mut self) {
            if self.config.debug {
                println!("opcode: SUB");
            }
            unimplemented!("opcode not implemented: SUB");
        }

        fn subi(&mut self) {
            if self.config.debug {
                println!("opcode: SUBI");
            }
            unimplemented!("opcode not implemented: SUBI");
        }

        fn subu(&mut self) {
            if self.config.debug {
                println!("opcode: SUBU");
            }
            unimplemented!("opcode not implemented: SUBU");
        }

        fn subui(&mut self) {
            if self.config.debug {
                println!("opcode: SUBUI");
            }
            unimplemented!("opcode not implemented: SUBUI");
        }

        fn add2u(&mut self) {
            if self.config.debug {
                println!("opcode: ADD2U");
            }
            unimplemented!("opcode not implemented: ADD2U");
        }

        fn add2ui(&mut self) {
            if self.config.debug {
                println!("opcode: ADD2UI");
            }
            unimplemented!("opcode not implemented: ADD2UI");
        }

        fn add4u(&mut self) {
            if self.config.debug {
                println!("opcode: ADD4U");
            }
            unimplemented!("opcode not implemented: ADD4U");
        }

        fn add4ui(&mut self) {
            if self.config.debug {
                println!("opcode: ADD4UI");
            }
            unimplemented!("opcode not implemented: ADD4UI");
        }

        fn add8u(&mut self) {
            if self.config.debug {
                println!("opcode: ADD8U");
            }
            unimplemented!("opcode not implemented: ADD8U");
        }

        fn add8ui(&mut self) {
            if self.config.debug {
                println!("opcode: ADD8UI");
            }
            unimplemented!("opcode not implemented: ADD8UI");
        }

        fn add16u(&mut self) {
            if self.config.debug {
                println!("opcode: ADD16U");
            }
            unimplemented!("opcode not implemented: ADD16U");
        }

        fn add16ui(&mut self) {
            if self.config.debug {
                println!("opcode: ADD16UI");
            }
            unimplemented!("opcode not implemented: ADD16UI");
        }

        fn cmp(&mut self) {
            if self.config.debug {
                println!("opcode: CMP");
            }
            unimplemented!("opcode not implemented: CMP");
        }

        fn cmpi(&mut self) {
            if self.config.debug {
                println!("opcode: CMPI");
            }
            unimplemented!("opcode not implemented: CMPI");
        }

        fn cmpu(&mut self) {
            if self.config.debug {
                println!("opcode: CMPU");
            }
            unimplemented!("opcode not implemented: CMPU");
        }

        fn cmpui(&mut self) {
            if self.config.debug {
                println!("opcode: CMPUI");
            }
            unimplemented!("opcode not implemented: CMPUI");
        }

        fn neg(&mut self) {
            if self.config.debug {
                println!("opcode: NEG");
            }
            unimplemented!("opcode not implemented: NEG");
        }

        fn negi(&mut self) {
            if self.config.debug {
                println!("opcode: NEGI");
            }
            unimplemented!("opcode not implemented: NEGI");
        }

        fn negu(&mut self) {
            if self.config.debug {
                println!("opcode: NEGU");
            }
            unimplemented!("opcode not implemented: NEGU");
        }

        fn negui(&mut self) {
            if self.config.debug {
                println!("opcode: NEGUI");
            }
            unimplemented!("opcode not implemented: NEGUI");
        }

        fn sl(&mut self) {
            if self.config.debug {
                println!("opcode: SL");
            }
            unimplemented!("opcode not implemented: SL");
        }

        fn sli(&mut self) {
            if self.config.debug {
                println!("opcode: SLI");
            }
            unimplemented!("opcode not implemented: SLI");
        }

        fn slu(&mut self) {
            if self.config.debug {
                println!("opcode: SLU");
            }
            unimplemented!("opcode not implemented: SLU");
        }

        fn slui(&mut self) {
            if self.config.debug {
                println!("opcode: SLUI");
            }
            unimplemented!("opcode not implemented: SLUI");
        }

        fn sr(&mut self) {
            if self.config.debug {
                println!("opcode: SR");
            }
            unimplemented!("opcode not implemented: SR");
        }

        fn sri(&mut self) {
            if self.config.debug {
                println!("opcode: SRI");
            }
            unimplemented!("opcode not implemented: SRI");
        }

        fn sru(&mut self) {
            if self.config.debug {
                println!("opcode: SRU");
            }
            unimplemented!("opcode not implemented: SRU");
        }

        fn srui(&mut self) {
            if self.config.debug {
                println!("opcode: SRUI");
            }
            unimplemented!("opcode not implemented: SRUI");
        }

        fn bn(&mut self) {
            if self.config.debug {
                println!("opcode: BN");
            }
            unimplemented!("opcode not implemented: BN");
        }

        fn bnb(&mut self) {
            if self.config.debug {
                println!("opcode: BNB");
            }
            unimplemented!("opcode not implemented: BNB");
        }

        fn bz(&mut self) {
            if self.config.debug {
                println!("opcode: BZ");
            }
            unimplemented!("opcode not implemented: BZ");
        }

        fn bzb(&mut self) {
            if self.config.debug {
                println!("opcode: BZB");
            }
            unimplemented!("opcode not implemented: BZB");
        }

        fn bp(&mut self) {
            if self.config.debug {
                println!("opcode: BP");
            }
            unimplemented!("opcode not implemented: BP");
        }

        fn bpb(&mut self) {
            if self.config.debug {
                println!("opcode: BPB");
            }
            unimplemented!("opcode not implemented: BPB");
        }

        fn bod(&mut self) {
            if self.config.debug {
                println!("opcode: BOD");
            }
            unimplemented!("opcode not implemented: BOD");
        }

        fn bodb(&mut self) {
            if self.config.debug {
                println!("opcode: BODB");
            }
            unimplemented!("opcode not implemented: BODB");
        }

        fn bnn(&mut self) {
            if self.config.debug {
                println!("opcode: BNN");
            }
            unimplemented!("opcode not implemented: BNN");
        }

        fn bnnb(&mut self) {
            if self.config.debug {
                println!("opcode: BNNB");
            }
            unimplemented!("opcode not implemented: BNNB");
        }

        fn bnz(&mut self) {
            if self.config.debug {
                println!("opcode: BNZ");
            }
            unimplemented!("opcode not implemented: BNZ");
        }

        fn bnzb(&mut self) {
            if self.config.debug {
                println!("opcode: BNZB");
            }
            unimplemented!("opcode not implemented: BNZB");
        }

        fn bnp(&mut self) {
            if self.config.debug {
                println!("opcode: BNP");
            }
            unimplemented!("opcode not implemented: BNP");
        }

        fn bnpb(&mut self) {
            if self.config.debug {
                println!("opcode: BNPB");
            }
            unimplemented!("opcode not implemented: BNPB");
        }

        fn bev(&mut self) {
            if self.config.debug {
                println!("opcode: BEV");
            }
            unimplemented!("opcode not implemented: BEV");
        }

        fn bevb(&mut self) {
            if self.config.debug {
                println!("opcode: BEVB");
            }
            unimplemented!("opcode not implemented: BEVB");
        }

        fn pbn(&mut self) {
            if self.config.debug {
                println!("opcode: PBN");
            }
            unimplemented!("opcode not implemented: PBN");
        }

        fn pbnb(&mut self) {
            if self.config.debug {
                println!("opcode: PBNB");
            }
            unimplemented!("opcode not implemented: PBNB");
        }

        fn pbz(&mut self) {
            if self.config.debug {
                println!("opcode: PBZ");
            }
            unimplemented!("opcode not implemented: PBZ");
        }

        fn pbzb(&mut self) {
            if self.config.debug {
                println!("opcode: PBZB");
            }
            unimplemented!("opcode not implemented: PBZB");
        }

        fn pbp(&mut self) {
            if self.config.debug {
                println!("opcode: PBP");
            }
            unimplemented!("opcode not implemented: PBP");
        }

        fn pbpb(&mut self) {
            if self.config.debug {
                println!("opcode: PBPB");
            }
            unimplemented!("opcode not implemented: PBPB");
        }

        fn pbod(&mut self) {
            if self.config.debug {
                println!("opcode: PBOD");
            }
            unimplemented!("opcode not implemented: PBOD");
        }

        fn pbodb(&mut self) {
            if self.config.debug {
                println!("opcode: PBODB");
            }
            unimplemented!("opcode not implemented: PBODB");
        }

        fn pbnn(&mut self) {
            if self.config.debug {
                println!("opcode: PBNN");
            }
            unimplemented!("opcode not implemented: PBNN");
        }

        fn pbnnb(&mut self) {
            if self.config.debug {
                println!("opcode: PBNNB");
            }
            unimplemented!("opcode not implemented: PBNNB");
        }

        fn pbnz(&mut self) {
            if self.config.debug {
                println!("opcode: PBNZ");
            }
            unimplemented!("opcode not implemented: PBNZ");
        }

        fn pbnzb(&mut self) {
            if self.config.debug {
                println!("opcode: PBNZB");
            }
            unimplemented!("opcode not implemented: PBNZB");
        }

        fn pbnp(&mut self) {
            if self.config.debug {
                println!("opcode: PBNP");
            }
            unimplemented!("opcode not implemented: PBNP");
        }

        fn pbnpb(&mut self) {
            if self.config.debug {
                println!("opcode: PBNPB");
            }
            unimplemented!("opcode not implemented: PBNPB");
        }

        fn pbev(&mut self) {
            if self.config.debug {
                println!("opcode: PBEV");
            }
            unimplemented!("opcode not implemented: PBEV");
        }

        fn pbevb(&mut self) {
            if self.config.debug {
                println!("opcode: PBEVB");
            }
            unimplemented!("opcode not implemented: PBEVB");
        }

        fn csn(&mut self) {
            if self.config.debug {
                println!("opcode: CSN");
            }
            unimplemented!("opcode not implemented: CSN");
        }

        fn csni(&mut self) {
            if self.config.debug {
                println!("opcode: CSNI");
            }
            unimplemented!("opcode not implemented: CSNI");
        }

        fn csz(&mut self) {
            if self.config.debug {
                println!("opcode: CSZ");
            }
            unimplemented!("opcode not implemented: CSZ");
        }

        fn cszi(&mut self) {
            if self.config.debug {
                println!("opcode: CSZI");
            }
            unimplemented!("opcode not implemented: CSZI");
        }

        fn csp(&mut self) {
            if self.config.debug {
                println!("opcode: CSP");
            }
            unimplemented!("opcode not implemented: CSP");
        }

        fn cspi(&mut self) {
            if self.config.debug {
                println!("opcode: CSPI");
            }
            unimplemented!("opcode not implemented: CSPI");
        }

        fn csod(&mut self) {
            if self.config.debug {
                println!("opcode: CSOD");
            }
            unimplemented!("opcode not implemented: CSOD");
        }

        fn csodi(&mut self) {
            if self.config.debug {
                println!("opcode: CSODI");
            }
            unimplemented!("opcode not implemented: CSODI");
        }

        fn csnn(&mut self) {
            if self.config.debug {
                println!("opcode: CSNN");
            }
            unimplemented!("opcode not implemented: CSNN");
        }

        fn csnni(&mut self) {
            if self.config.debug {
                println!("opcode: CSNNI");
            }
            unimplemented!("opcode not implemented: CSNNI");
        }

        fn csnz(&mut self) {
            if self.config.debug {
                println!("opcode: CSNZ");
            }
            unimplemented!("opcode not implemented: CSNZ");
        }

        fn csnzi(&mut self) {
            if self.config.debug {
                println!("opcode: CSNZI");
            }
            unimplemented!("opcode not implemented: CSNZI");
        }

        fn csnp(&mut self) {
            if self.config.debug {
                println!("opcode: CSNP");
            }
            unimplemented!("opcode not implemented: CSNP");
        }

        fn csnpi(&mut self) {
            if self.config.debug {
                println!("opcode: CSNPI");
            }
            unimplemented!("opcode not implemented: CSNPI");
        }

        fn csev(&mut self) {
            if self.config.debug {
                println!("opcode: CSEV");
            }
            unimplemented!("opcode not implemented: CSEV");
        }

        fn csevi(&mut self) {
            if self.config.debug {
                println!("opcode: CSEVI");
            }
            unimplemented!("opcode not implemented: CSEVI");
        }

        fn zsn(&mut self) {
            if self.config.debug {
                println!("opcode: ZSN");
            }
            unimplemented!("opcode not implemented: ZSN");
        }

        fn zsni(&mut self) {
            if self.config.debug {
                println!("opcode: ZSNI");
            }
            unimplemented!("opcode not implemented: ZSNI");
        }

        fn zsz(&mut self) {
            if self.config.debug {
                println!("opcode: ZSZ");
            }
            unimplemented!("opcode not implemented: ZSZ");
        }

        fn zszi(&mut self) {
            if self.config.debug {
                println!("opcode: ZSZI");
            }
            unimplemented!("opcode not implemented: ZSZI");
        }

        fn zsp(&mut self) {
            if self.config.debug {
                println!("opcode: ZSP");
            }
            unimplemented!("opcode not implemented: ZSP");
        }

        fn zspi(&mut self) {
            if self.config.debug {
                println!("opcode: ZSPI");
            }
            unimplemented!("opcode not implemented: ZSPI");
        }

        fn zsod(&mut self) {
            if self.config.debug {
                println!("opcode: ZSOD");
            }
            unimplemented!("opcode not implemented: ZSOD");
        }

        fn zsodi(&mut self) {
            if self.config.debug {
                println!("opcode: ZSODI");
            }
            unimplemented!("opcode not implemented: ZSODI");
        }

        fn zsnn(&mut self) {
            if self.config.debug {
                println!("opcode: ZSNN");
            }
            unimplemented!("opcode not implemented: ZSNN");
        }

        fn zsnni(&mut self) {
            if self.config.debug {
                println!("opcode: ZSNNI");
            }
            unimplemented!("opcode not implemented: ZSNNI");
        }

        fn zsnz(&mut self) {
            if self.config.debug {
                println!("opcode: ZSNZ");
            }
            unimplemented!("opcode not implemented: ZSNZ");
        }

        fn zsnzi(&mut self) {
            if self.config.debug {
                println!("opcode: ZSNZI");
            }
            unimplemented!("opcode not implemented: ZSNZI");
        }

        fn zsnp(&mut self) {
            if self.config.debug {
                println!("opcode: ZSNP");
            }
            unimplemented!("opcode not implemented: ZSNP");
        }

        fn zsnpi(&mut self) {
            if self.config.debug {
                println!("opcode: ZSNPI");
            }
            unimplemented!("opcode not implemented: ZSNPI");
        }

        fn zsev(&mut self) {
            if self.config.debug {
                println!("opcode: ZSEV");
            }
            unimplemented!("opcode not implemented: ZSEV");
        }

        fn zsevi(&mut self) {
            if self.config.debug {
                println!("opcode: ZSEVI");
            }
            unimplemented!("opcode not implemented: ZSEVI");
        }

        fn ldb(&mut self) {
            if self.config.debug {
                println!("opcode: LDB");
            }
            unimplemented!("opcode not implemented: LDB");
        }

        fn ldbi(&mut self) {
            if self.config.debug {
                println!("opcode: LDBI");
            }
            unimplemented!("opcode not implemented: LDBI");
        }

        fn ldbu(&mut self) {
            if self.config.debug {
                println!("opcode: LDBU");
            }
            unimplemented!("opcode not implemented: LDBU");
        }

        fn ldbui(&mut self) {
            if self.config.debug {
                println!("opcode: LDBUI");
            }
            unimplemented!("opcode not implemented: LDBUI");
        }

        fn ldw(&mut self) {
            if self.config.debug {
                println!("opcode: LDW");
            }
            unimplemented!("opcode not implemented: LDW");
        }

        fn ldwi(&mut self) {
            if self.config.debug {
                println!("opcode: LDWI");
            }
            unimplemented!("opcode not implemented: LDWI");
        }

        fn ldwu(&mut self) {
            if self.config.debug {
                println!("opcode: LDWU");
            }
            unimplemented!("opcode not implemented: LDWU");
        }

        fn ldwui(&mut self) {
            if self.config.debug {
                println!("opcode: LDWUI");
            }
            unimplemented!("opcode not implemented: LDWUI");
        }

        fn ldt(&mut self) {
            if self.config.debug {
                println!("opcode: LDT");
            }
            unimplemented!("opcode not implemented: LDT");
        }

        fn ldti(&mut self) {
            if self.config.debug {
                println!("opcode: LDTI");
            }
            unimplemented!("opcode not implemented: LDTI");
        }

        fn ldtu(&mut self) {
            if self.config.debug {
                println!("opcode: LDTU");
            }
            unimplemented!("opcode not implemented: LDTU");
        }

        fn ldtui(&mut self) {
            if self.config.debug {
                println!("opcode: LDTUI");
            }
            unimplemented!("opcode not implemented: LDTUI");
        }

        fn ldo(&mut self) {
            if self.config.debug {
                println!("opcode: LDO");
            }
            unimplemented!("opcode not implemented: LDO");
        }

        fn ldoi(&mut self) {
            if self.config.debug {
                println!("opcode: LDOI");
            }
            unimplemented!("opcode not implemented: LDOI");
        }

        fn ldou(&mut self) {
            if self.config.debug {
                println!("opcode: LDOU");
            }
            unimplemented!("opcode not implemented: LDOU");
        }

        fn ldoui(&mut self) {
            if self.config.debug {
                println!("opcode: LDOUI");
            }
            unimplemented!("opcode not implemented: LDOUI");
        }

        fn ldsf(&mut self) {
            if self.config.debug {
                println!("opcode: LDSF");
            }
            unimplemented!("opcode not implemented: LDSF");
        }

        fn ldsfi(&mut self) {
            if self.config.debug {
                println!("opcode: LDSFI");
            }
            unimplemented!("opcode not implemented: LDSFI");
        }

        fn ldht(&mut self) {
            if self.config.debug {
                println!("opcode: LDHT");
            }
            unimplemented!("opcode not implemented: LDHT");
        }

        fn ldhti(&mut self) {
            if self.config.debug {
                println!("opcode: LDHTI");
            }
            unimplemented!("opcode not implemented: LDHTI");
        }

        fn cswap(&mut self) {
            if self.config.debug {
                println!("opcode: CSWAP");
            }
            unimplemented!("opcode not implemented: CSWAP");
        }

        fn cswapi(&mut self) {
            if self.config.debug {
                println!("opcode: CSWAPI");
            }
            unimplemented!("opcode not implemented: CSWAPI");
        }

        fn ldunc(&mut self) {
            if self.config.debug {
                println!("opcode: LDUNC");
            }
            unimplemented!("opcode not implemented: LDUNC");
        }

        fn ldunci(&mut self) {
            if self.config.debug {
                println!("opcode: LDUNCI");
            }
            unimplemented!("opcode not implemented: LDUNCI");
        }

        fn ldvts(&mut self) {
            if self.config.debug {
                println!("opcode: LDVTS");
            }
            unimplemented!("opcode not implemented: LDVTS");
        }

        fn ldvtsi(&mut self) {
            if self.config.debug {
                println!("opcode: LDVTSI");
            }
            unimplemented!("opcode not implemented: LDVTSI");
        }

        fn preld(&mut self) {
            if self.config.debug {
                println!("opcode: PRELD");
            }
            unimplemented!("opcode not implemented: PRELD");
        }

        fn preldi(&mut self) {
            if self.config.debug {
                println!("opcode: PRELDI");
            }
            unimplemented!("opcode not implemented: PRELDI");
        }

        fn prego(&mut self) {
            if self.config.debug {
                println!("opcode: PREGO");
            }
            unimplemented!("opcode not implemented: PREGO");
        }

        fn pregoi(&mut self) {
            if self.config.debug {
                println!("opcode: PREGOI");
            }
            unimplemented!("opcode not implemented: PREGOI");
        }

        fn go(&mut self) {
            if self.config.debug {
                println!("opcode: GO");
            }
            unimplemented!("opcode not implemented: GO");
        }

        fn goi(&mut self) {
            if self.config.debug {
                println!("opcode: GOI");
            }
            unimplemented!("opcode not implemented: GOI");
        }

        fn stb(&mut self) {
            if self.config.debug {
                println!("opcode: STB");
            }
            unimplemented!("opcode not implemented: STB");
        }

        fn stbi(&mut self) {
            if self.config.debug {
                println!("opcode: STBI");
            }
            unimplemented!("opcode not implemented: STBI");
        }

        fn stbu(&mut self) {
            if self.config.debug {
                println!("opcode: STBU");
            }
            unimplemented!("opcode not implemented: STBU");
        }

        fn stbui(&mut self) {
            if self.config.debug {
                println!("opcode: STBUI");
            }
            unimplemented!("opcode not implemented: STBUI");
        }

        fn stw(&mut self) {
            if self.config.debug {
                println!("opcode: STW");
            }
            unimplemented!("opcode not implemented: STW");
        }

        fn stwi(&mut self) {
            if self.config.debug {
                println!("opcode: STWI");
            }
            unimplemented!("opcode not implemented: STWI")
        }

        fn stwu(&mut self) {
            if self.config.debug {
                println!("opcode: STWU");
            }
            unimplemented!("opcode not implemented: STWU");
        }

        fn stwui(&mut self) {
            if self.config.debug {
                println!("opcode: STWUI");
            }
            unimplemented!("opcode not implemented: STWUI");
        }

        fn stt(&mut self) {
            if self.config.debug {
                println!("opcode: STT");
            }
            unimplemented!("opcode not implemented: STT");
        }

        fn stti(&mut self) {
            if self.config.debug {
                println!("opcode: STTI");
            }
            unimplemented!("opcode not implemented: STTI");
        }

        fn sttu(&mut self) {
            if self.config.debug {
                println!("opcode: STTU");
            }
            unimplemented!("opcode not implemented: STTU");
        }

        fn sttui(&mut self) {
            if self.config.debug {
                println!("opcode: STTUI");
            }
            unimplemented!("opcode not implemented: STTUI");
        }

        fn sto(&mut self) {
            if self.config.debug {
                println!("opcode: STO");
            }
            unimplemented!("opcode not implemented: STO");
        }

        fn stoi(&mut self) {
            if self.config.debug {
                println!("opcode: STOI");
            }
            unimplemented!("opcode not implemented: STOI");
        }

        fn stou(&mut self) {
            if self.config.debug {
                println!("opcode: STOU");
            }
            unimplemented!("opcode not implemented: STOU");
        }

        fn stoui(&mut self) {
            if self.config.debug {
                println!("opcode: STOUI");
            }
            unimplemented!("opcode not implemented: STOUI");
        }

        fn stsf(&mut self) {
            if self.config.debug {
                println!("opcode: STSF");
            }
            unimplemented!("opcode not implemented: STSF");
        }

        fn stsfi(&mut self) {
            if self.config.debug {
                println!("opcode: STSFI");
            }
            unimplemented!("opcode not implemented: STSFI");
        }

        fn stht(&mut self) {
            if self.config.debug {
                println!("opcode: STHT");
            }
            unimplemented!("opcode not implemented: STHT");
        }

        fn sthti(&mut self) {
            if self.config.debug {
                println!("opcode: STHTI");
            }
            unimplemented!("opcode not implemented: STHTI");
        }

        fn stco(&mut self) {
            if self.config.debug {
                println!("opcode: STCO");
            }
            unimplemented!("opcode not implemented: STCO");
        }

        fn stcoii(&mut self) {
            if self.config.debug {
                println!("opcode: STCOII");
            }
            unimplemented!("opcode not implemented: STCOII");
        }

        fn stunc(&mut self) {
            if self.config.debug {
                println!("opcode: STUNC");
            }
            unimplemented!("opcode not implemented: STUNC");
        }

        fn stunci(&mut self) {
            if self.config.debug {
                println!("opcode: STUNCI");
            }
            unimplemented!("opcode not implemented: STUNCI");
        }

        fn syncd(&mut self) {
            if self.config.debug {
                println!("opcode: SYNCD");
            }
            unimplemented!("opcode not implemented: SYNCD");
        }

        fn syncdi(&mut self) {
            if self.config.debug {
                println!("opcode: SYNCDI");
            }
            unimplemented!("opcode not implemented: SYNCDI");
        }

        fn prest(&mut self) {
            if self.config.debug {
                println!("opcode: PREST");
            }
            unimplemented!("opcode not implemented: PREST");
        }

        fn presti(&mut self) {
            if self.config.debug {
                println!("opcode: PRESTI");
            }
            unimplemented!("opcode not implemented: PRESTI");
        }

        fn syncid(&mut self) {
            if self.config.debug {
                println!("opcode: SYNCID");
            }
            unimplemented!("opcode not implemented: SYNCID");
        }

        fn syncidi(&mut self) {
            if self.config.debug {
                println!("opcode: SYNCIDI");
            }
            unimplemented!("opcode not implemented: SYNCIDI");
        }

        fn pushgo(&mut self) {
            if self.config.debug {
                println!("opcode: PUSHGO");
            }
            unimplemented!("opcode not implemented: PUSHGO");
        }

        fn pushgoi(&mut self) {
            if self.config.debug {
                println!("opcode: PUSHGOI");
            }
            unimplemented!("opcode not implemented: PUSHGOI");
        }

        fn or(&mut self) {
            if self.config.debug {
                println!("opcode: OR");
            }
            unimplemented!("opcode not implemented: OR");
        }

        fn ori(&mut self) {
            if self.config.debug {
                println!("opcode: ORI");
            }
            unimplemented!("opcode not implemented: ORI");
        }

        fn orn(&mut self) {
            if self.config.debug {
                println!("opcode: ORN");
            }
            unimplemented!("opcode not implemented: ORN");
        }

        fn orni(&mut self) {
            if self.config.debug {
                println!("opcode: ORNI");
            }
            unimplemented!("opcode not implemented: ORNI");
        }

        fn nor(&mut self) {
            if self.config.debug {
                println!("opcode: NOR");
            }
            unimplemented!("opcode not implemented: NOR");
        }

        fn nori(&mut self) {
            if self.config.debug {
                println!("opcode: NORI");
            }
            unimplemented!("opcode not implemented: NORI");
        }

        fn xor(&mut self) {
            if self.config.debug {
                println!("opcode: XOR");
            }
            unimplemented!("opcode not implemented: XOR");
        }

        fn xori(&mut self) {
            if self.config.debug {
                println!("opcode: XORI");
            }
            unimplemented!("opcode not implemented: XORI");
        }

        fn and(&mut self) {
            if self.config.debug {
                println!("opcode: AND");
            }
            unimplemented!("opcode not implemented: AND");
        }

        fn andi(&mut self) {
            if self.config.debug {
                println!("opcode: ANDI");
            }
            unimplemented!("opcode not implemented: ANDI");
        }

        fn andn(&mut self) {
            if self.config.debug {
                println!("opcode: ANDN");
            }
            unimplemented!("opcode not implemented: ANDN");
        }

        fn andni(&mut self) {
            if self.config.debug {
                println!("opcode: ANDNI");
            }
            unimplemented!("opcode not implemented: ANDNI");
        }

        fn nand(&mut self) {
            if self.config.debug {
                println!("opcode: NAND");
            }
            unimplemented!("opcode not implemented: NAND");
        }

        fn nandi(&mut self) {
            if self.config.debug {
                println!("opcode: NANDI");
            }
            unimplemented!("opcode not implemented: NANDI");
        }

        fn nxor(&mut self) {
            if self.config.debug {
                println!("opcode: NXOR");
            }
            unimplemented!("opcode not implemented: NXOR");
        }

        fn nxori(&mut self) {
            if self.config.debug {
                println!("opcode: NXORI");
            }
            unimplemented!("opcode not implemented: NXORI");
        }

        fn bdif(&mut self) {
            if self.config.debug {
                println!("opcode: BDIF");
            }
            unimplemented!("opcode not implemented: BDIF");
        }

        fn bdifi(&mut self) {
            if self.config.debug {
                println!("opcode: BDIFI");
            }
            unimplemented!("opcode not implemented: BDIFI");
        }

        fn wdif(&mut self) {
            if self.config.debug {
                println!("opcode: WDIF");
            }
            unimplemented!("opcode not implemented: WDIF");
        }

        fn wdifi(&mut self) {
            if self.config.debug {
                println!("opcode: WDIFI");
            }
            unimplemented!("opcode not implemented: WDIFI");
        }

        fn tdif(&mut self) {
            if self.config.debug {
                println!("opcode: TDIF");
            }
            unimplemented!("opcode not implemented: TDIF");
        }

        fn tdifi(&mut self) {
            if self.config.debug {
                println!("opcode: TDIFI");
            }
            unimplemented!("opcode not implemented: TDIFI");
        }

        fn odif(&mut self) {
            if self.config.debug {
                println!("opcode: ODIF");
            }
            unimplemented!("opcode not implemented: ODIF");
        }

        fn odifi(&mut self) {
            if self.config.debug {
                println!("opcode: ODIFI");
            }
            unimplemented!("opcode not implemented: ODIFI");
        }

        fn mux(&mut self) {
            if self.config.debug {
                println!("opcode: MUX");
            }
            unimplemented!("opcode not implemented: MUX");
        }

        fn muxi(&mut self) {
            if self.config.debug {
                println!("opcode: MUXI");
            }
            unimplemented!("opcode not implemented: MUXI");
        }

        fn sadd(&mut self) {
            if self.config.debug {
                println!("opcode: SADD");
            }
            unimplemented!("opcode not implemented: SADD");
        }

        fn saddi(&mut self) {
            if self.config.debug {
                println!("opcode: SADDI");
            }
            unimplemented!("opcode not implemented: SADDI");
        }

        fn mor(&mut self) {
            if self.config.debug {
                println!("opcode: MOR");
            }
            unimplemented!("opcode not implemented: MOR");
        }

        fn mori(&mut self) {
            if self.config.debug {
                println!("opcode: MORI");
            }
            unimplemented!("opcode not implemented: MORI");
        }

        fn mxor(&mut self) {
            if self.config.debug {
                println!("opcode: MXOR");
            }
            unimplemented!("opcode not implemented: MXOR");
        }

        fn mxori(&mut self) {
            if self.config.debug {
                println!("opcode: MXORI");
            }
            unimplemented!("opcode not implemented: MXORI");
        }

        fn seth(&mut self) {
            if self.config.debug {
                println!("opcode: SETH");
            }
            unimplemented!("opcode not implemented: SETH");
        }

        fn setmh(&mut self) {
            if self.config.debug {
                println!("opcode: SETMH");
            }
            unimplemented!("opcode not implemented: SETMH");
        }

        fn setml(&mut self) {
            if self.config.debug {
                println!("opcode: SETML");
            }
            unimplemented!("opcode not implemented: SETML");
        }

        fn setl(&mut self) {
            if self.config.debug {
                println!("opcode: SETL");
            }
            unimplemented!("opcode not implemented: SETL");
        }

        fn inch(&mut self) {
            if self.config.debug {
                println!("opcode: INCH");
            }
            unimplemented!("opcode not implemented: INCH");
        }

        fn incmh(&mut self) {
            if self.config.debug {
                println!("opcode: INCMH");
            }
            unimplemented!("opcode not implemented: INCMH");
        }

        fn incml(&mut self) {
            if self.config.debug {
                println!("opcode: INCML");
            }
            unimplemented!("opcode not implemented: INCML");
        }

        fn incl(&mut self) {
            if self.config.debug {
                println!("opcode: INCL");
            }
            unimplemented!("opcode not implemented: INCL");
        }

        fn orh(&mut self) {
            if self.config.debug {
                println!("opcode: ORH");
            }
            unimplemented!("opcode not implemented: ORH");
        }

        fn ormh(&mut self) {
            if self.config.debug {
                println!("opcode: ORMH");
            }
            unimplemented!("opcode not implemented: ORMH");
        }

        fn orml(&mut self) {
            if self.config.debug {
                println!("opcode: ORML");
            }
            unimplemented!("opcode not implemented: ORML");
        }

        fn orl(&mut self) {
            if self.config.debug {
                println!("opcode: ORL");
            }
            unimplemented!("opcode not implemented: ORL");
        }

        fn andnh(&mut self) {
            if self.config.debug {
                println!("opcode: ANDNH");
            }
            unimplemented!("opcode not implemented: ANDNH");
        }

        fn andnmh(&mut self) {
            if self.config.debug {
                println!("opcode: ANDNMH");
            }
            unimplemented!("opcode not implemented: ANDNMH");
        }

        fn andnml(&mut self) {
            if self.config.debug {
                println!("opcode: ANDNML");
            }
            unimplemented!("opcode not implemented: ANDNML");
        }

        fn andnl(&mut self) {
            if self.config.debug {
                println!("opcode: ANDNL");
            }
            unimplemented!("opcode not implemented: ANDNL");
        }

        fn jmp(&mut self) {
            if self.config.debug {
                println!("opcode: JMP");
            }
            unimplemented!("opcode not implemented: JMP");
        }

        fn jmpb(&mut self) {
            if self.config.debug {
                println!("opcode: JMPB");
            }
            unimplemented!("opcode not implemented: JMPB");
        }

        fn pushj(&mut self) {
            if self.config.debug {
                println!("opcode: PUSHJ");
            }
            unimplemented!("opcode not implemented: PUSHJ");
        }

        fn pushjb(&mut self) {
            if self.config.debug {
                println!("opcode: PUSHJB");
            }
            unimplemented!("opcode not implemented: PUSHJB");
        }

        fn geta(&mut self) {
            if self.config.debug {
                println!("opcode: GETA");
            }
            unimplemented!("opcode not implemented: GETA");
        }

        fn getab(&mut self) {
            if self.config.debug {
                println!("opcode: GETAB");
            }
            unimplemented!("opcode not implemented: GETAB");
        }

        fn put(&mut self) {
            if self.config.debug {
                println!("opcode: PUT");
            }
            unimplemented!("opcode not implemented: PUT");
        }

        fn puti(&mut self) {
            if self.config.debug {
                println!("opcode: PUTI");
            }
            unimplemented!("opcode not implemented: PUTI");
        }

        fn pop(&mut self) {
            if self.config.debug {
                println!("opcode: POP");
            }
            unimplemented!("opcode not implemented: POP");
        }

        fn resume(&mut self) {
            if self.config.debug {
                println!("opcode: RESUME");
            }
            unimplemented!("opcode not implemented: RESUME");
        }

        fn save(&mut self) {
            if self.config.debug {
                println!("opcode: SAVE");
            }
            unimplemented!("opcode not implemented: SAVE");
        }

        fn unsave(&mut self) {
            if self.config.debug {
                println!("opcode: UNSAVE");
            }
            unimplemented!("opcode not implemented: UNSAVE");
        }

        fn sync(&mut self) {
            if self.config.debug {
                println!("opcode: SYNC");
            }
            unimplemented!("opcode not implemented: SYNC");
        }

        fn swym(&mut self) {
            if self.config.debug {
                println!("opcode: SWYM");
            }
            unimplemented!("opcode not implemented: SWYM");
        }

        fn get(&mut self) {
            if self.config.debug {
                println!("opcode: GET");
            }
            unimplemented!("opcode not implemented: GET");
        }

        fn trip(&mut self) {
            if self.config.debug {
                println!("opcode: TRIP");
            }
            unimplemented!("opcode not implemented: TRIP");
        }
    }
}

mod nnix {}

fn parse_cli_args() -> Result<mmix::Config, String> {
    let args: Vec<String> = env::args().collect();
    println!("Args: {args:?}");
    let config = mmix::Config { debug: true };
    Ok(config)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Parse CLI arguments
    let config = parse_cli_args()?;

    // Initialize the virtual machine.
    let mut vm = mmix::Mmixvm::new(config);

    // Load the program
    todo!();
    // let program = mmix::Program::new();
    // vm.load_program(program);

    // Run the program
    vm.run()?;
}
