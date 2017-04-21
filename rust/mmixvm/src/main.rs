use std::env;
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

/**
# MMIX
*/

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
    TRIP = 0xFF
}

struct Config {
    debug: bool
}

struct VM {
    rA: u64,
    rB: u64,
    rC: u64,
    rD: u64,
    rE: u64,
    rF: u64,
    rG: u64,
    rH: u64,
    rI: u64,
    rJ: u64,
    rK: u64,
    rL: u64,
    rM: u64,
    rN: u64,
    rO: u64,
    rP: u64,
    rQ: u64,
    rR: u64,
    rS: u64,
    rT: u64,
    rU: u64,
    rV: u64,
    rW: u64,
    rX: u64,
    rY: u64,
    rZ: u64,
    rBB: u64,
    rTT: u64,
    rWW: u64,
    rXX: u64,
    rYY: u64,
    rZZ: u64,
    r: [u64; 256],
    memory: Vec<u64>
}

fn main() {
    let mut config = Config{debug: false};

    // parse opts & configure
    let args: Vec<String> = env::args().collect();
    println!("path: {}", args[0]);
    println!("{:?} arguments: {:?}", args.len() - 1, &args[1..]);
    for arg in args {
        match &arg[..] {
            "--debug" =>
                {
                    config.debug = true
                },
            _ =>
                {
                }
        }
    }

    // open program file
    let path = Path::new("test.mmo");
    let display = path.display();
    let mut file =
        match File::open(&path) {
            Err(why) => panic!("couldn't open {}: {}", display, why.description()),
            Ok(file) => file,
        };

    // initialize vm
    let mut vm = VM{
            rA: 0,
            rB: 0, // Bootstrap register (trip). When tripping, rB ← $255 and
                   // $255 ← rJ, thus saving rJ in a general register.
            rC: 0, // Cycle counter.
            rD: 0, // Dividend register. Unsigned integer divide uses this as
                   // the left half of the 128-bit input that is to be divided
                   // by the other operand.
            rE: 0, // Epsilon register. Used for floating comparisons with
                   // respect to epsilon.
            rF: 0,
            rG: 0,
            rH: 0, // Himult register. Used to store the left half of the
                   // 128-bit result of unsigned integer multiplication.
            rI: 0, // interval counter
            rJ: 0, // return-jump
            rK: 0, // interrupt mask
            rL: 0,
            rM: 0, // multiplex mask
            rN: 0, // serial number
            rO: 0, // register stack offset
            rP: 0,
            rQ: 0,
            rR: 0, // remainder
            rS: 0, // register stack pointer
            rT: 0, // trap address
            rU: 0,
            rV: 0,
            rW: 0,
            rX: 0,
            rY: 0,
            rZ: 0,
            rBB: 0, // bootstrap (trap)
            rTT: 0, // dynamic trap address
            rWW: 0,
            rXX: 0,
            rYY: 0,
            rZZ: 0,
            r: [0; 256],
            memory: vec![0; 1024]
        };
    let mut word: u32;
    let mut opcode: u8;
    let mut opX: u8;
    let mut opY: u8;
    let mut opZ: u8;
    let mut opYZ: u16;
    let mut opXYZ: u32;

    loop {
        // fetch
        let mut buf: [u8; 8] = [0; 8];
        match file.read(&mut buf) {
            Err(why) =>
                {
                    panic!("couldn't read {}: {}", display, why.description());
                }
            Ok(uz) =>
                {
                    println!("file read successfully: bytes: {}", uz);
                }
        }
        break;

        // decode
        // match opcode {
        //     _ => println!("unknown opcode")
        // }

        // execute
    }
    // cleanup
}
