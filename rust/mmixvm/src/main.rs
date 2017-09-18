use std::env;
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::mem;
use std::path::Path;

/**
# MMIX
*/

struct Config {
    debug: bool,
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
    TRIP = 0xFF
}

fn from_u8(n: u8) -> Opcode {
    unsafe { mem::transmute(n) }
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
    memory: [u64; 1048576]
}

impl Default for VM {
    fn default() -> VM {
        VM {
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
            r: [0u64; 256],
            memory: [0u64; 1048576]
        }
    }
}

fn trap(config: &Config) {
    if config.debug { println!("opcode: TRAP"); }
    panic!("opcode not implemented: TRAP");
}

fn fcmp(config: &Config) {
    if config.debug { println!("opcode: FCMP"); }
    panic!("opcode not implemented: FCMP");
}

fn fun(config: &Config) {
    if config.debug { println!("opcode: FUN"); }
    panic!("opcode not implemented: FUN");
}

fn feql(config: &Config) {
    if config.debug { println!("opcode: FEQL"); }
    panic!("opcode not implemented: FEQL");
}

fn fadd(config: &Config) {
    if config.debug { println!("opcode: FADD"); }
    panic!("opcode not implemented: FADD");
}

fn fix(config: &Config) {
    if config.debug { println!("opcode: FIX"); }
    panic!("opcode not implemented: FIX");
}

fn fsub(config: &Config) {
    if config.debug { println!("opcode: FSUB"); }
    panic!("opcode not implemented: FSUB");
}

fn fixu(config: &Config) {
    if config.debug { println!("opcode: FIXU"); }
    panic!("opcode not implemented: FIXU");
}

fn flot(config: &Config) {
    if config.debug { println!("opcode: FLOT"); }
    panic!("opcode not implemented: FLOT");
}

fn floti(config: &Config) {
    if config.debug { println!("opcode: FLOTI"); }
    panic!("opcode not implemented: FLOTI");
}

fn flotu(config: &Config) {
    if config.debug { println!("opcode: FLOTU"); }
    panic!("opcode not implemented: FLOTU");
}

fn flotui(config: &Config) {
    if config.debug { println!("opcode: FLOTUI"); }
    panic!("opcode not implemented: FLOTUI");
}

fn sflot(config: &Config) {
    if config.debug { println!("opcode: SFLOT"); }
    panic!("opcode not implemented: SFLOT");
}

fn sfloti(config: &Config) {
    if config.debug { println!("opcode: SFLOTI"); }
    panic!("opcode not implemented: SFLOTI");
}

fn sflotu(config: &Config) {
    if config.debug { println!("opcode: SFLOTU"); }
    panic!("opcode not implemented: SFLOTU");
}

fn sflotui(config: &Config) {
    if config.debug { println!("opcode: SFLOTUI"); }
    panic!("opcode not implemented: SFLOTUI");
}

fn fmul(config: &Config) {
    if config.debug { println!("opcode: FMUL"); }
    panic!("opcode not implemented: FMUL");
}

fn fcmpe(config: &Config) {
    if config.debug { println!("opcode: FCMPE"); }
    panic!("opcode not implemented: FCMPE");
}

fn fune(config: &Config) {
    if config.debug { println!("opcode: FUNE"); }
    panic!("opcode not implemented: FUNE");
}

fn feqle(config: &Config) {
    if config.debug { println!("opcode: FEQLE"); }
    panic!("opcode not implemented: FEQLE")
}

fn fdiv(config: &Config) {
    if config.debug { println!("opcode: FDIV"); }
    panic!("opcode not implemented: FDIV");
}

fn fsqrt(config: &Config) {
    if config.debug { println!("opcode: FSQRT"); }
    panic!("opcode not implemented: FSQRT");
}

fn frem(config: &Config) {
    if config.debug { println!("opcode: FREM"); }
    panic!("opcode not implemented: FREM");
}

fn fint(config: &Config) {
    if config.debug { println!("opcode: FINT"); }
    panic!("opcode not implemented: FINT");
}

fn mul(config: &Config) {
    if config.debug { println!("opcode: MUL"); }
    panic!("opcode not implemented: MUL");
}

fn muli(config: &Config) {
    if config.debug { println!("opcode: MULI"); }
    panic!("opcode not implemented: MULI");
}

fn mulu(config: &Config) {
    if config.debug { println!("opcode: MULU"); }
    panic!("opcode not implemented: MULU");
}

fn mului(config: &Config) {
    if config.debug { println!("opcode: MULUI"); }
    panic!("opcode not implemented: MULUI");
}

fn div(config: &Config) {
    if config.debug { println!("opcode: DIV"); }
    panic!("opcode not implemented: DIV");
}

fn divi(config: &Config) {
    if config.debug { println!("opcode: DIVI"); }
    panic!("opcode not implemented: DIVI");
}

fn divu(config: &Config) {
    if config.debug { println!("opcode: DIVU"); }
    panic!("opcode not implemented: DIVU");
}

fn divui(config: &Config) {
    if config.debug { println!("opcode: DIVUI"); }
    panic!("opcode not implemented: DIVUI");
}

fn add(config: &Config) {
    if config.debug { println!("opcode: ADD"); }
    panic!("opcode not implemented: ADD");
}

fn addi(config: &Config) {
    if config.debug { println!("opcode: ADDI"); }
    panic!("opcode not implemented: ADDI");
}

fn addu(config: &Config) {
    if config.debug { println!("opcode: ADDU"); }
    panic!("opcode not implemented: ADDU");
}

fn addui(config: &Config) {
    if config.debug { println!("opcode: ADDUI"); }
    panic!("opcode not implemented: ADDUI");
}

fn sub(config: &Config) {
    if config.debug { println!("opcode: SUB"); }
    panic!("opcode not implemented: SUB");
}

fn subi(config: &Config) {
    if config.debug { println!("opcode: SUBI"); }
    panic!("opcode not implemented: SUBI");
}

fn subu(config: &Config) {
    if config.debug { println!("opcode: SUBU"); }
    panic!("opcode not implemented: SUBU");
}

fn subui(config: &Config) {
    if config.debug { println!("opcode: SUBUI"); }
    panic!("opcode not implemented: SUBUI");
}

fn add2u(config: &Config) {
    if config.debug { println!("opcode: ADD2U"); }
    panic!("opcode not implemented: ADD2U");
}

fn add2ui(config: &Config) {
    if config.debug { println!("opcode: ADD2UI"); }
    panic!("opcode not implemented: ADD2UI");
}

fn add4u(config: &Config) {
    if config.debug { println!("opcode: ADD4U"); }
    panic!("opcode not implemented: ADD4U");
}

fn add4ui(config: &Config) {
    if config.debug { println!("opcode: ADD4UI"); }
    panic!("opcode not implemented: ADD4UI");
}

fn add8u(config: &Config) {
    if config.debug { println!("opcode: ADD8U"); }
    panic!("opcode not implemented: ADD8U");
}

fn add8ui(config: &Config) {
    if config.debug { println!("opcode: ADD8UI"); }
    panic!("opcode not implemented: ADD8UI");
}

fn add16u(config: &Config) {
    if config.debug { println!("opcode: ADD16U"); }
    panic!("opcode not implemented: ADD16U");
}

fn add16ui(config: &Config) {
    if config.debug { println!("opcode: ADD16UI"); }
    panic!("opcode not implemented: ADD16UI");
}

fn cmp(config: &Config) {
    if config.debug { println!("opcode: CMP"); }
    panic!("opcode not implemented: CMP");
}

fn cmpi(config: &Config) {
    if config.debug { println!("opcode: CMPI"); }
    panic!("opcode not implemented: CMPI");
}

fn cmpu(config: &Config) {
    if config.debug { println!("opcode: CMPU"); }
    panic!("opcode not implemented: CMPU");
}

fn cmpui(config: &Config) {
    if config.debug { println!("opcode: CMPUI"); }
    panic!("opcode not implemented: CMPUI");
}

fn neg(config: &Config) {
    if config.debug { println!("opcode: NEG"); }
    panic!("opcode not implemented: NEG");
}

fn negi(config: &Config) {
    if config.debug { println!("opcode: NEGI"); }
    panic!("opcode not implemented: NEGI");
}

fn negu(config: &Config) {
    if config.debug { println!("opcode: NEGU"); }
    panic!("opcode not implemented: NEGU");
}

fn negui(config: &Config) {
    if config.debug { println!("opcode: NEGUI"); }
    panic!("opcode not implemented: NEGUI");
}

fn sl(config: &Config) {
    if config.debug { println!("opcode: SL"); }
    panic!("opcode not implemented: SL");
}

fn sli(config: &Config) {
    if config.debug { println!("opcode: SLI"); }
    panic!("opcode not implemented: SLI");
}

fn slu(config: &Config) {
    if config.debug { println!("opcode: SLU"); }
    panic!("opcode not implemented: SLU");
}

fn slui(config: &Config) {
    if config.debug { println!("opcode: SLUI"); }
    panic!("opcode not implemented: SLUI");
}

fn sr(config: &Config) {
    if config.debug { println!("opcode: SR"); }
    panic!("opcode not implemented: SR");
}

fn sri(config: &Config) {
    if config.debug { println!("opcode: SRI"); }
    panic!("opcode not implemented: SRI");
}

fn sru(config: &Config) {
    if config.debug { println!("opcode: SRU"); }
    panic!("opcode not implemented: SRU");
}

fn srui(config: &Config) {
    if config.debug { println!("opcode: SRUI"); }
    panic!("opcode not implemented: SRUI");
}

fn bn(config: &Config) {
    if config.debug { println!("opcode: BN"); }
    panic!("opcode not implemented: BN");
}

fn bnb(config: &Config) {
    if config.debug { println!("opcode: BNB"); }
    panic!("opcode not implemented: BNB");
}

fn bz(config: &Config) {
    if config.debug { println!("opcode: BZ"); }
    panic!("opcode not implemented: BZ");
}

fn bzb(config: &Config) {
    if config.debug { println!("opcode: BZB"); }
    panic!("opcode not implemented: BZB");
}

fn bp(config: &Config) {
    if config.debug { println!("opcode: BP"); }
    panic!("opcode not implemented: BP");
}

fn bpb(config: &Config) {
    if config.debug { println!("opcode: BPB"); }
    panic!("opcode not implemented: BPB");
}

fn bod(config: &Config) {
    if config.debug { println!("opcode: BOD"); }
    panic!("opcode not implemented: BOD");
}

fn bodb(config: &Config) {
    if config.debug { println!("opcode: BODB"); }
    panic!("opcode not implemented: BODB");
}

fn bnn(config: &Config) {
    if config.debug { println!("opcode: BNN"); }
    panic!("opcode not implemented: BNN");
}

fn bnnb(config: &Config) {
    if config.debug { println!("opcode: BNNB"); }
    panic!("opcode not implemented: BNNB");
}

fn bnz(config: &Config) {
    if config.debug { println!("opcode: BNZ"); }
    panic!("opcode not implemented: BNZ");
}

fn bnzb(config: &Config) {
    if config.debug { println!("opcode: BNZB"); }
    panic!("opcode not implemented: BNZB");
}

fn bnp(config: &Config) {
    if config.debug { println!("opcode: BNP"); }
    panic!("opcode not implemented: BNP");
}

fn bnpb(config: &Config) {
    if config.debug { println!("opcode: BNPB"); }
    panic!("opcode not implemented: BNPB");
}

fn bev(config: &Config) {
    if config.debug { println!("opcode: BEV"); }
    panic!("opcode not implemented: BEV");
}

fn bevb(config: &Config) {
    if config.debug { println!("opcode: BEVB"); }
    panic!("opcode not implemented: BEVB");
}

fn pbn(config: &Config) {
    if config.debug { println!("opcode: PBN"); }
    panic!("opcode not implemented: PBN");
}

fn pbnb(config: &Config) {
    if config.debug { println!("opcode: PBNB"); }
    panic!("opcode not implemented: PBNB");
}

fn pbz(config: &Config) {
    if config.debug { println!("opcode: PBZ"); }
    panic!("opcode not implemented: PBZ");
}

fn pbzb(config: &Config) {
    if config.debug { println!("opcode: PBZB"); }
    panic!("opcode not implemented: PBZB");
}

fn pbp(config: &Config) {
    if config.debug { println!("opcode: PBP"); }
    panic!("opcode not implemented: PBP");
}

fn pbpb(config: &Config) {
    if config.debug { println!("opcode: PBPB"); }
    panic!("opcode not implemented: PBPB");
}

fn pbod(config: &Config) {
    if config.debug { println!("opcode: PBOD"); }
    panic!("opcode not implemented: PBOD");
}

fn pbodb(config: &Config) {
    if config.debug { println!("opcode: PBODB"); }
    panic!("opcode not implemented: PBODB");
}

fn pbnn(config: &Config) {
    if config.debug { println!("opcode: PBNN"); }
    panic!("opcode not implemented: PBNN");
}

fn pbnnb(config: &Config) {
    if config.debug { println!("opcode: PBNNB"); }
    panic!("opcode not implemented: PBNNB");
}

fn pbnz(config: &Config) {
    if config.debug { println!("opcode: PBNZ"); }
    panic!("opcode not implemented: PBNZ");
}

fn pbnzb(config: &Config) {
    if config.debug { println!("opcode: PBNZB"); }
    panic!("opcode not implemented: PBNZB");
}

fn pbnp(config: &Config) {
    if config.debug { println!("opcode: PBNP"); }
    panic!("opcode not implemented: PBNP");
}

fn pbnpb(config: &Config) {
    if config.debug { println!("opcode: PBNPB"); }
    panic!("opcode not implemented: PBNPB");
}

fn pbev(config: &Config) {
    if config.debug { println!("opcode: PBEV"); }
    panic!("opcode not implemented: PBEV");
}

fn pbevb(config: &Config) {
    if config.debug { println!("opcode: PBEVB"); }
    panic!("opcode not implemented: PBEVB");
}

fn csn(config: &Config) {
    if config.debug { println!("opcode: CSN"); }
    panic!("opcode not implemented: CSN");
}

fn csni(config: &Config) {
    if config.debug { println!("opcode: CSNI"); }
    panic!("opcode not implemented: CSNI");
}

fn csz(config: &Config) {
    if config.debug { println!("opcode: CSZ"); }
    panic!("opcode not implemented: CSZ");
}

fn cszi(config: &Config) {
    if config.debug { println!("opcode: CSZI"); }
    panic!("opcode not implemented: CSZI");
}

fn csp(config: &Config) {
    if config.debug { println!("opcode: CSP"); }
    panic!("opcode not implemented: CSP");
}

fn cspi(config: &Config) {
    if config.debug { println!("opcode: CSPI"); }
    panic!("opcode not implemented: CSPI");
}

fn csod(config: &Config) {
    if config.debug { println!("opcode: CSOD"); }
    panic!("opcode not implemented: CSOD");
}

fn csodi(config: &Config) {
    if config.debug { println!("opcode: CSODI"); }
    panic!("opcode not implemented: CSODI");
}

fn csnn(config: &Config) {
    if config.debug { println!("opcode: CSNN"); }
    panic!("opcode not implemented: CSNN");
}

fn csnni(config: &Config) {
    if config.debug { println!("opcode: CSNNI"); }
    panic!("opcode not implemented: CSNNI");
}

fn csnz(config: &Config) {
    if config.debug { println!("opcode: CSNZ"); }
    panic!("opcode not implemented: CSNZ");
}

fn csnzi(config: &Config) {
    if config.debug { println!("opcode: CSNZI"); }
    panic!("opcode not implemented: CSNZI");
}

fn csnp(config: &Config) {
    if config.debug { println!("opcode: CSNP"); }
    panic!("opcode not implemented: CSNP");
}

fn csnpi(config: &Config) {
    if config.debug { println!("opcode: CSNPI"); }
    panic!("opcode not implemented: CSNPI");
}

fn csev(config: &Config) {
    if config.debug { println!("opcode: CSEV"); }
    panic!("opcode not implemented: CSEV");
}

fn csevi(config: &Config) {
    if config.debug { println!("opcode: CSEVI"); }
    panic!("opcode not implemented: CSEVI");
}

fn zsn(config: &Config) {
    if config.debug { println!("opcode: ZSN"); }
    panic!("opcode not implemented: ZSN");
}

fn zsni(config: &Config) {
    if config.debug { println!("opcode: ZSNI"); }
    panic!("opcode not implemented: ZSNI");
}

fn zsz(config: &Config) {
    if config.debug { println!("opcode: ZSZ"); }
    panic!("opcode not implemented: ZSZ");
}

fn zszi(config: &Config) {
    if config.debug { println!("opcode: ZSZI"); }
    panic!("opcode not implemented: ZSZI");
}

fn zsp(config: &Config) {
    if config.debug { println!("opcode: ZSP"); }
    panic!("opcode not implemented: ZSP");
}

fn zspi(config: &Config) {
    if config.debug { println!("opcode: ZSPI"); }
    panic!("opcode not implemented: ZSPI");
}

fn zsod(config: &Config) {
    if config.debug { println!("opcode: ZSOD"); }
    panic!("opcode not implemented: ZSOD");
}

fn zsodi(config: &Config) {
    if config.debug { println!("opcode: ZSODI"); }
    panic!("opcode not implemented: ZSODI");
}

fn zsnn(config: &Config) {
    if config.debug { println!("opcode: ZSNN"); }
    panic!("opcode not implemented: ZSNN");
}

fn zsnni(config: &Config) {
    if config.debug { println!("opcode: ZSNNI"); }
    panic!("opcode not implemented: ZSNNI");
}

fn zsnz(config: &Config) {
    if config.debug { println!("opcode: ZSNZ"); }
    panic!("opcode not implemented: ZSNZ");
}

fn zsnzi(config: &Config) {
    if config.debug { println!("opcode: ZSNZI"); }
    panic!("opcode not implemented: ZSNZI");
}

fn zsnp(config: &Config) {
    if config.debug { println!("opcode: ZSNP"); }
    panic!("opcode not implemented: ZSNP");
}

fn zsnpi(config: &Config) {
    if config.debug { println!("opcode: ZSNPI"); }
    panic!("opcode not implemented: ZSNPI");
}

fn zsev(config: &Config) {
    if config.debug { println!("opcode: ZSEV"); }
    panic!("opcode not implemented: ZSEV");
}

fn zsevi(config: &Config) {
    if config.debug { println!("opcode: ZSEVI"); }
    panic!("opcode not implemented: ZSEVI");
}

fn ldb(config: &Config) {
    if config.debug { println!("opcode: LDB"); }
    panic!("opcode not implemented: LDB");
}

fn ldbi(config: &Config) {
    if config.debug { println!("opcode: LDBI"); }
    panic!("opcode not implemented: LDBI");
}

fn ldbu(config: &Config) {
    if config.debug { println!("opcode: LDBU"); }
    panic!("opcode not implemented: LDBU");
}

fn ldbui(config: &Config) {
    if config.debug { println!("opcode: LDBUI"); }
    panic!("opcode not implemented: LDBUI");
}

fn ldw(config: &Config) {
    if config.debug { println!("opcode: LDW"); }
    panic!("opcode not implemented: LDW");
}

fn ldwi(config: &Config) {
    if config.debug { println!("opcode: LDWI"); }
    panic!("opcode not implemented: LDWI");
}

fn ldwu(config: &Config) {
    if config.debug { println!("opcode: LDWU"); }
    panic!("opcode not implemented: LDWU");
}

fn ldwui(config: &Config) {
    if config.debug { println!("opcode: LDWUI"); }
    panic!("opcode not implemented: LDWUI");
}

fn ldt(config: &Config) {
    if config.debug { println!("opcode: LDT"); }
    panic!("opcode not implemented: LDT");
}

fn ldti(config: &Config) {
    if config.debug { println!("opcode: LDTI"); }
    panic!("opcode not implemented: LDTI");
}

fn ldtu(config: &Config) {
    if config.debug { println!("opcode: LDTU"); }
    panic!("opcode not implemented: LDTU");
}

fn ldtui(config: &Config) {
    if config.debug { println!("opcode: LDTUI"); }
    panic!("opcode not implemented: LDTUI");
}

fn ldo(config: &Config) {
    if config.debug { println!("opcode: LDO"); }
    panic!("opcode not implemented: LDO");
}

fn ldoi(config: &Config) {
    if config.debug { println!("opcode: LDOI"); }
    panic!("opcode not implemented: LDOI");
}

fn ldou(config: &Config) {
    if config.debug { println!("opcode: LDOU"); }
    panic!("opcode not implemented: LDOU");
}

fn ldoui(config: &Config) {
    if config.debug { println!("opcode: LDOUI"); }
    panic!("opcode not implemented: LDOUI");
}

fn ldsf(config: &Config) {
    if config.debug { println!("opcode: LDSF"); }
    panic!("opcode not implemented: LDSF");
}

fn ldsfi(config: &Config) {
    if config.debug { println!("opcode: LDSFI"); }
    panic!("opcode not implemented: LDSFI");
}

fn ldht(config: &Config) {
    if config.debug { println!("opcode: LDHT"); }
    panic!("opcode not implemented: LDHT");
}

fn ldhti(config: &Config) {
    if config.debug { println!("opcode: LDHTI"); }
    panic!("opcode not implemented: LDHTI");
}

fn cswap(config: &Config) {
    if config.debug { println!("opcode: CSWAP"); }
    panic!("opcode not implemented: CSWAP");
}

fn cswapi(config: &Config) {
    if config.debug { println!("opcode: CSWAPI"); }
    panic!("opcode not implemented: CSWAPI");
}

fn ldunc(config: &Config) {
    if config.debug { println!("opcode: LDUNC"); }
    panic!("opcode not implemented: LDUNC");
}

fn ldunci(config: &Config) {
    if config.debug { println!("opcode: LDUNCI"); }
    panic!("opcode not implemented: LDUNCI");
}

fn ldvts(config: &Config) {
    if config.debug { println!("opcode: LDVTS"); }
    panic!("opcode not implemented: LDVTS");
}

fn ldvtsi(config: &Config) {
    if config.debug { println!("opcode: LDVTSI"); }
    panic!("opcode not implemented: LDVTSI");
}

fn preld(config: &Config) {
    if config.debug { println!("opcode: PRELD"); }
    panic!("opcode not implemented: PRELD");
}

fn preldi(config: &Config) {
    if config.debug { println!("opcode: PRELDI"); }
    panic!("opcode not implemented: PRELDI");
}

fn prego(config: &Config) {
    if config.debug { println!("opcode: PREGO"); }
    panic!("opcode not implemented: PREGO");
}

fn pregoi(config: &Config) {
    if config.debug { println!("opcode: PREGOI"); }
    panic!("opcode not implemented: PREGOI");
}

fn go(config: &Config) {
    if config.debug { println!("opcode: GO"); }
    panic!("opcode not implemented: GO");
}

fn goi(config: &Config) {
    if config.debug { println!("opcode: GOI"); }
    panic!("opcode not implemented: GOI");
}

fn stb(config: &Config) {
    if config.debug { println!("opcode: STB"); }
    panic!("opcode not implemented: STB");
}

fn stbi(config: &Config) {
    if config.debug { println!("opcode: STBI"); }
    panic!("opcode not implemented: STBI");
}

fn stbu(config: &Config) {
    if config.debug { println!("opcode: STBU"); }
    panic!("opcode not implemented: STBU");
}

fn stbui(config: &Config) {
    if config.debug { println!("opcode: STBUI"); }
    panic!("opcode not implemented: STBUI");
}

fn stw(config: &Config) {
    if config.debug { println!("opcode: STW"); }
    panic!("opcode not implemented: STW");
}

fn stwi(config: &Config) {
    if config.debug { println!("opcode: STWI"); }
    panic!("opcode not implemented: STWI")
}

fn stwu(config: &Config) {
    if config.debug { println!("opcode: STWU"); }
    panic!("opcode not implemented: STWU");
}

fn stwui(config: &Config) {
    if config.debug { println!("opcode: STWUI"); }
    panic!("opcode not implemented: STWUI");
}

fn stt(config: &Config) {
    if config.debug { println!("opcode: STT"); }
    panic!("opcode not implemented: STT");
}

fn stti(config: &Config) {
    if config.debug { println!("opcode: STTI"); }
    panic!("opcode not implemented: STTI");
}

fn sttu(config: &Config) {
    if config.debug { println!("opcode: STTU"); }
    panic!("opcode not implemented: STTU");
}

fn sttui(config: &Config) {
    if config.debug { println!("opcode: STTUI"); }
    panic!("opcode not implemented: STTUI");
}

fn sto(config: &Config) {
    if config.debug { println!("opcode: STO"); }
    panic!("opcode not implemented: STO");
}

fn stoi(config: &Config) {
    if config.debug { println!("opcode: STOI"); }
    panic!("opcode not implemented: STOI");
}

fn stou(config: &Config) {
    if config.debug { println!("opcode: STOU"); }
    panic!("opcode not implemented: STOU");
}

fn stoui(config: &Config) {
    if config.debug { println!("opcode: STOUI"); }
    panic!("opcode not implemented: STOUI");
}

fn stsf(config: &Config) {
    if config.debug { println!("opcode: STSF"); }
    panic!("opcode not implemented: STSF");
}

fn stsfi(config: &Config) {
    if config.debug { println!("opcode: STSFI"); }
    panic!("opcode not implemented: STSFI");
}

fn stht(config: &Config) {
    if config.debug { println!("opcode: STHT"); }
    panic!("opcode not implemented: STHT");
}

fn sthti(config: &Config) {
    if config.debug { println!("opcode: STHTI"); }
    panic!("opcode not implemented: STHTI");
}

fn stco(config: &Config) {
    if config.debug { println!("opcode: STCO"); }
    panic!("opcode not implemented: STCO");
}

fn stcoii(config: &Config) {
    if config.debug { println!("opcode: STCOII"); }
    panic!("opcode not implemented: STCOII");
}

fn stunc(config: &Config) {
    if config.debug { println!("opcode: STUNC"); }
    panic!("opcode not implemented: STUNC");
}

fn stunci(config: &Config) {
    if config.debug { println!("opcode: STUNCI"); }
    panic!("opcode not implemented: STUNCI");
}

fn syncd(config: &Config) {
    if config.debug { println!("opcode: SYNCD"); }
    panic!("opcode not implemented: SYNCD");
}

fn syncdi(config: &Config) {
    if config.debug { println!("opcode: SYNCDI"); }
    panic!("opcode not implemented: SYNCDI");
}

fn prest(config: &Config) {
    if config.debug { println!("opcode: PREST"); }
    panic!("opcode not implemented: PREST");
}

fn presti(config: &Config) {
    if config.debug { println!("opcode: PRESTI"); }
    panic!("opcode not implemented: PRESTI");
}

fn syncid(config: &Config) {
    if config.debug { println!("opcode: SYNCID"); }
    panic!("opcode not implemented: SYNCID");
}

fn syncidi(config: &Config) {
    if config.debug { println!("opcode: SYNCIDI"); }
    panic!("opcode not implemented: SYNCIDI");
}

fn pushgo(config: &Config) {
    if config.debug { println!("opcode: PUSHGO"); }
    panic!("opcode not implemented: PUSHGO");
}

fn pushgoi(config: &Config) {
    if config.debug { println!("opcode: PUSHGOI"); }
    panic!("opcode not implemented: PUSHGOI");
}

fn or(config: &Config) {
    if config.debug { println!("opcode: OR"); }
    panic!("opcode not implemented: OR");
}

fn ori(config: &Config) {
    if config.debug { println!("opcode: ORI"); }
    panic!("opcode not implemented: ORI");
}

fn orn(config: &Config) {
    if config.debug { println!("opcode: ORN"); }
    panic!("opcode not implemented: ORN");
}

fn orni(config: &Config) {
    if config.debug { println!("opcode: ORNI"); }
    panic!("opcode not implemented: ORNI");
}

fn nor(config: &Config) {
    if config.debug { println!("opcode: NOR"); }
    panic!("opcode not implemented: NOR");
}

fn nori(config: &Config) {
    if config.debug { println!("opcode: NORI"); }
    panic!("opcode not implemented: NORI");
}

fn xor(config: &Config) {
    if config.debug { println!("opcode: XOR"); }
    panic!("opcode not implemented: XOR");
}

fn xori(config: &Config) {
    if config.debug { println!("opcode: XORI"); }
    panic!("opcode not implemented: XORI");
}

fn and(config: &Config) {
    if config.debug { println!("opcode: AND"); }
    panic!("opcode not implemented: AND");
}

fn andi(config: &Config) {
    if config.debug { println!("opcode: ANDI"); }
    panic!("opcode not implemented: ANDI");
}

fn andn(config: &Config) {
    if config.debug { println!("opcode: ANDN"); }
    panic!("opcode not implemented: ANDN");
}

fn andni(config: &Config) {
    if config.debug { println!("opcode: ANDNI"); }
    panic!("opcode not implemented: ANDNI");
}

fn nand(config: &Config) {
    if config.debug { println!("opcode: NAND"); }
    panic!("opcode not implemented: NAND");
}

fn nandi(config: &Config) {
    if config.debug { println!("opcode: NANDI"); }
    panic!("opcode not implemented: NANDI");
}

fn nxor(config: &Config) {
    if config.debug { println!("opcode: NXOR"); }
    panic!("opcode not implemented: NXOR");
}

fn nxori(config: &Config) {
    if config.debug { println!("opcode: NXORI"); }
    panic!("opcode not implemented: NXORI");
}

fn bdif(config: &Config) {
    if config.debug { println!("opcode: BDIF"); }
    panic!("opcode not implemented: BDIF");
}

fn bdifi(config: &Config) {
    if config.debug { println!("opcode: BDIFI"); }
    panic!("opcode not implemented: BDIFI");
}

fn wdif(config: &Config) {
    if config.debug { println!("opcode: WDIF"); }
    panic!("opcode not implemented: WDIF");
}

fn wdifi(config: &Config) {
    if config.debug { println!("opcode: WDIFI"); }
    panic!("opcode not implemented: WDIFI");
}

fn tdif(config: &Config) {
    if config.debug { println!("opcode: TDIF"); }
    panic!("opcode not implemented: TDIF");
}

fn tdifi(config: &Config) {
    if config.debug { println!("opcode: TDIFI"); }
    panic!("opcode not implemented: TDIFI");
}

fn odif(config: &Config) {
    if config.debug { println!("opcode: ODIF"); }
    panic!("opcode not implemented: ODIF");
}

fn odifi(config: &Config) {
    if config.debug { println!("opcode: ODIFI"); }
    panic!("opcode not implemented: ODIFI");
}

fn mux(config: &Config) {
    if config.debug { println!("opcode: MUX"); }
    panic!("opcode not implemented: MUX");
}

fn muxi(config: &Config) {
    if config.debug { println!("opcode: MUXI"); }
    panic!("opcode not implemented: MUXI");
}

fn sadd(config: &Config) {
    if config.debug { println!("opcode: SADD"); }
    panic!("opcode not implemented: SADD");
}

fn saddi(config: &Config) {
    if config.debug { println!("opcode: SADDI"); }
    panic!("opcode not implemented: SADDI");
}

fn mor(config: &Config) {
    if config.debug { println!("opcode: MOR"); }
    panic!("opcode not implemented: MOR");
}

fn mori(config: &Config) {
    if config.debug { println!("opcode: MORI"); }
    panic!("opcode not implemented: MORI");
}

fn mxor(config: &Config) {
    if config.debug { println!("opcode: MXOR"); }
    panic!("opcode not implemented: MXOR");
}

fn mxori(config: &Config) {
    if config.debug { println!("opcode: MXORI"); }
    panic!("opcode not implemented: MXORI");
}

fn seth(config: &Config) {
    if config.debug { println!("opcode: SETH"); }
    panic!("opcode not implemented: SETH");
}

fn setmh(config: &Config) {
    if config.debug { println!("opcode: SETMH"); }
    panic!("opcode not implemented: SETMH");
}

fn setml(config: &Config) {
    if config.debug { println!("opcode: SETML"); }
    panic!("opcode not implemented: SETML");
}

fn setl(config: &Config) {
    if config.debug { println!("opcode: SETL"); }
    panic!("opcode not implemented: SETL");
}

fn inch(config: &Config) {
    if config.debug { println!("opcode: INCH"); }
    panic!("opcode not implemented: INCH");
}

fn incmh(config: &Config) {
    if config.debug { println!("opcode: INCMH"); }
    panic!("opcode not implemented: INCMH");
}

fn incml(config: &Config) {
    if config.debug { println!("opcode: INCML"); }
    panic!("opcode not implemented: INCML");
}

fn incl(config: &Config) {
    if config.debug { println!("opcode: INCL"); }
    panic!("opcode not implemented: INCL");
}

fn orh(config: &Config) {
    if config.debug { println!("opcode: ORH"); }
    panic!("opcode not implemented: ORH");
}

fn ormh(config: &Config) {
    if config.debug { println!("opcode: ORMH"); }
    panic!("opcode not implemented: ORMH");
}

fn orml(config: &Config) {
    if config.debug { println!("opcode: ORML"); }
    panic!("opcode not implemented: ORML");
}

fn orl(config: &Config) {
    if config.debug { println!("opcode: ORL"); }
    panic!("opcode not implemented: ORL");
}

fn andnh(config: &Config) {
    if config.debug { println!("opcode: ANDNH"); }
    panic!("opcode not implemented: ANDNH");
}

fn andnmh(config: &Config) {
    if config.debug { println!("opcode: ANDNMH"); }
    panic!("opcode not implemented: ANDNMH");
}

fn andnml(config: &Config) {
    if config.debug { println!("opcode: ANDNML"); }
    panic!("opcode not implemented: ANDNML");
}

fn andnl(config: &Config) {
    if config.debug { println!("opcode: ANDNL"); }
    panic!("opcode not implemented: ANDNL");
}

fn jmp(config: &Config) {
    if config.debug { println!("opcode: JMP"); }
    panic!("opcode not implemented: JMP");
}

fn jmpb(config: &Config) {
    if config.debug { println!("opcode: JMPB"); }
    panic!("opcode not implemented: JMPB");
}

fn pushj(config: &Config) {
    if config.debug { println!("opcode: PUSHJ"); }
    panic!("opcode not implemented: PUSHJ");
}

fn pushjb(config: &Config) {
    if config.debug { println!("opcode: PUSHJB"); }
    panic!("opcode not implemented: PUSHJB");
}

fn geta(config: &Config) {
    if config.debug { println!("opcode: GETA"); }
    panic!("opcode not implemented: GETA");
}

fn getab(config: &Config) {
    if config.debug { println!("opcode: GETAB"); }
    panic!("opcode not implemented: GETAB");
}

fn put(config: &Config) {
    if config.debug { println!("opcode: PUT"); }
    panic!("opcode not implemented: PUT");
}

fn puti(config: &Config) {
    if config.debug { println!("opcode: PUTI"); }
    panic!("opcode not implemented: PUTI");
}

fn pop(config: &Config) {
    if config.debug { println!("opcode: POP"); }
    panic!("opcode not implemented: POP");
}

fn resume(config: &Config) {
    if config.debug { println!("opcode: RESUME"); }
    panic!("opcode not implemented: RESUME");
}

fn save(config: &Config) {
    if config.debug { println!("opcode: SAVE"); }
    panic!("opcode not implemented: SAVE");
}

fn unsave(config: &Config) {
    if config.debug { println!("opcode: UNSAVE"); }
    panic!("opcode not implemented: UNSAVE");
}

fn sync(config: &Config) {
    if config.debug { println!("opcode: SYNC"); }
    panic!("opcode not implemented: SYNC");
}

fn swym(config: &Config) {
    if config.debug { println!("opcode: SWYM"); }
    panic!("opcode not implemented: SWYM");
}

fn get(config: &Config) {
    if config.debug { println!("opcode: GET"); }
    panic!("opcode not implemented: GET");
}

fn trip(config: &Config) {
    if config.debug { println!("opcode: TRIP"); }
    panic!("opcode not implemented: TRIP");
}

fn config() -> Config {
    // parse opts & configure
    let args: Vec<String> = env::args().collect();

    println!("path: {}", args[0]);
    println!("{:?} arguments: {:?}", args.len() - 1, &args[1..]);

    let mut config = Config {
        debug: false
    };

    for arg in args {
        match &arg[..] {
            "--debug" =>
                {
                    config.debug = true;
                },
            _ =>
                {
                    println!("Unknown configuration option: {}", arg);
                }
        }
    }
    config
}

fn code_file(config: &Config) -> File {
    // open program file
    let path = Path::new("test.mmo"); // TODO
    match File::open(&path) {
        Err(why) => {
            let display = path.display();
            panic!("couldn't open {}: {}", display, why.description())
        },
        Ok(file) => file
    }
}

fn main() {
    let config = config();
    let mut file = code_file(&config);

    let mut vm: VM;
    // let mut word: u32;
    let mut opcode: u8;
    // let mut opX: u8;
    // let mut opY: u8;
    // let mut opZ: u8;
    // let mut opYZ: u16;
    // let mut opXYZ: u32;

    loop {
        // fetch
        let mut buf: [u8; 8] = [0; 8];
        match file.read(&mut buf) {
            Err(why) =>
                {
                    panic!("couldn't read from file: {}", why.description());
                }
            Ok(uz) =>
                {
                    println!("file read successfully: {} bytes", uz);
                }
        }

        // decode & execute
        opcode = buf[0];
        match from_u8(opcode) {
            Opcode::TRAP => {
                trap(&config);
            }
            Opcode::FCMP => {
                fcmp(&config);
            }
            Opcode::FUN => {
                fun(&config);
            }
            Opcode::FEQL => {
                feql(&config);
            }
            Opcode::FADD => {
                fadd(&config);
            }
            Opcode::FIX => {
                fix(&config);
            }
            Opcode::FSUB => {
                fsub(&config);
            }
            Opcode::FIXU => {
                fixu(&config);
            }
            Opcode::FLOT => {
                flot(&config);
            }
            Opcode::FLOTI => {
                floti(&config);
            }
            Opcode::FLOTU => {
                flotu(&config);
            }
            Opcode::FLOTUI => {
                flotui(&config);
            }
            Opcode::SFLOT => {
                sflot(&config);
            }
            Opcode::SFLOTI => {
                sfloti(&config);
            }
            Opcode::SFLOTU => {
                sflotu(&config);
            }
            Opcode::SFLOTUI => {
                sflotui(&config);
            }
            Opcode::FMUL => {
                fmul(&config);
            }
            Opcode::FCMPE => {
                fcmpe(&config);
            }
            Opcode::FUNE => {
                fune(&config);
            }
            Opcode::FEQLE => {
                feqle(&config);
            }
            Opcode::FDIV => {
                fdiv(&config);
            }
            Opcode::FSQRT => {
                fsqrt(&config);
            }
            Opcode::FREM => {
                frem(&config);
            }
            Opcode::FINT => {
                fint(&config);
            }
            Opcode::MUL => {
                mul(&config);
            }
            Opcode::MULI => {
                muli(&config);
            }
            Opcode::MULU => {
                mulu(&config);
            }
            Opcode::MULUI => {
                mului(&config);
            }
            Opcode::DIV => {
                div(&config);
            }
            Opcode::DIVI => {
                divi(&config);
            }
            Opcode::DIVU => {
                divu(&config);
            }
            Opcode::DIVUI => {
                divui(&config);
            }
            Opcode::ADD => {
                add(&config);
            }
            Opcode::ADDI => {
                addi(&config);
            }
            Opcode::ADDU => {
                addu(&config);
            }
            Opcode::ADDUI => {
                addui(&config);
            }
            Opcode::SUB => {
                sub(&config);
            }
            Opcode::SUBI => {
                subi(&config);
            }
            Opcode::SUBU => {
                subu(&config);
            }
            Opcode::SUBUI => {
                subui(&config);
            }
            Opcode::ADD2U => {
                add2u(&config);
            }
            Opcode::ADD2UI => {
                add2ui(&config);
            }
            Opcode::ADD4U => {
                add4u(&config);
            }
            Opcode::ADD4UI => {
                add4ui(&config);
            }
            Opcode::ADD8U => {
                add8u(&config);
            }
            Opcode::ADD8UI => {
                add8ui(&config);
            }
            Opcode::ADD16U => {
                add16u(&config);
            }
            Opcode::ADD16UI => {
                add16ui(&config);
            }
            Opcode::CMP => {
                cmp(&config);
            }
            Opcode::CMPI => {
                cmpi(&config);
            }
            Opcode::CMPU => {
                cmpu(&config);
            }
            Opcode::CMPUI => {
                cmpui(&config);
            }
            Opcode::NEG => {
                neg(&config);
            }
            Opcode::NEGI => {
                negi(&config);
            }
            Opcode::NEGU => {
                negu(&config);
            }
            Opcode::NEGUI => {
                negui(&config);
            }
            Opcode::SL => {
                sl(&config);
            }
            Opcode::SLI => {
                sli(&config);
            }
            Opcode::SLU => {
                slu(&config);
            }
            Opcode::SLUI => {
                slui(&config);
            }
            Opcode::SR => {
                sr(&config);
            }
            Opcode::SRI => {
                sri(&config);
            }
            Opcode::SRU => {
                sru(&config);
            }
            Opcode::SRUI => {
                srui(&config);
            }
            Opcode::BN => {
                bn(&config);
            }
            Opcode::BNB => {
                bnb(&config);
            }
            Opcode::BZ => {
                bz(&config);
            }
            Opcode::BZB => {
                bzb(&config);
            }
            Opcode::BP => {
                bp(&config);
            }
            Opcode::BPB => {
                bpb(&config);
            }
            Opcode::BOD => {
                bod(&config);
            }
            Opcode::BODB => {
                bodb(&config);
            }
            Opcode::BNN => {
                bnn(&config);
            }
            Opcode::BNNB => {
                bnnb(&config);
            }
            Opcode::BNZ => {
                bnz(&config);
            }
            Opcode::BNZB => {
                bnzb(&config);
            }
            Opcode::BNP => {
                bnp(&config);
            }
            Opcode::BNPB => {
                bnpb(&config);
            }
            Opcode::BEV => {
                bev(&config);
            }
            Opcode::BEVB => {
                bevb(&config);
            }
            Opcode::PBN => {
                pbn(&config);
            }
            Opcode::PBNB => {
                pbnb(&config);
            }
            Opcode::PBZ => {
                pbz(&config);
            }
            Opcode::PBZB => {
                pbzb(&config);
            }
            Opcode::PBP => {
                pbp(&config);
            }
            Opcode::PBPB => {
                pbpb(&config);
            }
            Opcode::PBOD => {
                pbod(&config);
            }
            Opcode::PBODB => {
                pbodb(&config);
            }
            Opcode::PBNN => {
                pbnn(&config);
            }
            Opcode::PBNNB => {
                pbnnb(&config);
            }
            Opcode::PBNZ => {
                pbnz(&config);
            }
            Opcode::PBNZB => {
                pbnzb(&config);
            }
            Opcode::PBNP => {
                pbnp(&config);
            }
            Opcode::PBNPB => {
                pbnpb(&config);
            }
            Opcode::PBEV => {
                pbev(&config);
            }
            Opcode::PBEVB => {
                pbevb(&config);
            }
            Opcode::CSN => {
                csn(&config);
            }
            Opcode::CSNI => {
                csni(&config);
            }
            Opcode::CSZ => {
                csz(&config);
            }
            Opcode::CSZI => {
                cszi(&config);
            }
            Opcode::CSP => {
                csp(&config);
            }
            Opcode::CSPI => {
                cspi(&config);
            }
            Opcode::CSOD => {
                csod(&config);
            }
            Opcode::CSODI => {
                csodi(&config);
            }
            Opcode::CSNN => {
                csnn(&config);
            }
            Opcode::CSNNI => {
                csnni(&config);
            }
            Opcode::CSNZ => {
                csnz(&config);
            }
            Opcode::CSNZI => {
                csnzi(&config);
            }
            Opcode::CSNP => {
                csnp(&config);
            }
            Opcode::CSNPI => {
                csnpi(&config);
            }
            Opcode::CSEV => {
                csev(&config);
            }
            Opcode::CSEVI => {
                csevi(&config);
            }
            Opcode::ZSN => {
                zsn(&config);
            }
            Opcode::ZSNI => {
                zsni(&config);
            }
            Opcode::ZSZ => {
                zsz(&config);
            }
            Opcode::ZSZI => {
                zszi(&config);
            }
            Opcode::ZSP => {
                zsp(&config);
            }
            Opcode::ZSPI => {
                zspi(&config);
            }
            Opcode::ZSOD => {
                zsod(&config);
            }
            Opcode::ZSODI => {
                zsodi(&config);
            }
            Opcode::ZSNN => {
                zsnn(&config);
            }
            Opcode::ZSNNI => {
                zsnni(&config);
            }
            Opcode::ZSNZ => {
                zsnz(&config);
            }
            Opcode::ZSNZI => {
                zsnzi(&config);
            }
            Opcode::ZSNP => {
                zsnp(&config);
            }
            Opcode::ZSNPI => {
                zsnpi(&config);
            }
            Opcode::ZSEV => {
                zsev(&config);
            }
            Opcode::ZSEVI => {
                zsevi(&config);
            }
            Opcode::LDB => {
                ldb(&config);
            }
            Opcode::LDBI => {
                ldbi(&config);
            }
            Opcode::LDBU => {
                ldbu(&config);
            }
            Opcode::LDBUI => {
                ldbui(&config);
            }
            Opcode::LDW => {
                ldw(&config);
            }
            Opcode::LDWI => {
                ldwi(&config);
            }
            Opcode::LDWU => {
                ldwu(&config);
            }
            Opcode::LDWUI => {
                ldwui(&config);
            }
            Opcode::LDT => {
                ldt(&config);
            }
            Opcode::LDTI => {
                ldti(&config);
            }
            Opcode::LDTU => {
                ldtu(&config);
            }
            Opcode::LDTUI => {
                ldtui(&config);
            }
            Opcode::LDO => {
                ldo(&config);
            }
            Opcode::LDOI => {
                ldoi(&config);
            }
            Opcode::LDOU => {
                ldou(&config);
            }
            Opcode::LDOUI => {
                ldoui(&config);
            }
            Opcode::LDSF => {
                ldsf(&config);
            }
            Opcode::LDSFI => {
                ldsfi(&config);
            }
            Opcode::LDHT => {
                ldht(&config);
            }
            Opcode::LDHTI => {
                ldhti(&config);
            }
            Opcode::CSWAP => {
                cswap(&config);
            }
            Opcode::CSWAPI => {
                cswapi(&config);
            }
            Opcode::LDUNC => {
                ldunc(&config);
            }
            Opcode::LDUNCI => {
                ldunci(&config);
            }
            Opcode::LDVTS => {
                ldvts(&config);
            }
            Opcode::LDVTSI => {
                ldvtsi(&config);
            }
            Opcode::PRELD => {
                preld(&config);
            }
            Opcode::PRELDI => {
                preldi(&config);
            }
            Opcode::PREGO => {
                prego(&config);
            }
            Opcode::PREGOI => {
                pregoi(&config);
            }
            Opcode::GO => {
                go(&config);
            }
            Opcode::GOI => {
                goi(&config);
            }
            Opcode::STB => {
                stb(&config);
            }
            Opcode::STBI => {
                stbi(&config);
            }
            Opcode::STBU => {
                stbu(&config);
            }
            Opcode::STBUI => {
                stbui(&config);
            }
            Opcode::STW => {
                stw(&config);
            }
            Opcode::STWI => {
                stwi(&config);
            }
            Opcode::STWU => {
                stwu(&config);
            }
            Opcode::STWUI => {
                stwui(&config);
            }
            Opcode::STT => {
                stt(&config);
            }
            Opcode::STTI => {
                stti(&config);
            }
            Opcode::STTU => {
                sttu(&config);
            }
            Opcode::STTUI => {
                sttui(&config);
            }
            Opcode::STO => {
                sto(&config);
            }
            Opcode::STOI => {
                stoi(&config);
            }
            Opcode::STOU => {
                stou(&config);
            }
            Opcode::STOUI => {
                stoui(&config);
            }
            Opcode::STSF => {
                stsf(&config);
            }
            Opcode::STSFI => {
                stsfi(&config);
            }
            Opcode::STHT => {
                stht(&config);
            }
            Opcode::STHTI => {
                sthti(&config);
            }
            Opcode::STCO => {
                stco(&config);
            }
            Opcode::STCOII => {
                stcoii(&config);
            }
            Opcode::STUNC => {
                stunc(&config);
            }
            Opcode::STUNCI => {
                stunci(&config);
            }
            Opcode::SYNCD => {
                syncd(&config);
            }
            Opcode::SYNCDI => {
                syncdi(&config);
            }
            Opcode::PREST => {
                prest(&config);
            }
            Opcode::PRESTI => {
                presti(&config);
            }
            Opcode::SYNCID => {
                syncid(&config);
            }
            Opcode::SYNCIDI => {
                syncidi(&config);
            }
            Opcode::PUSHGO => {
                pushgo(&config);
            }
            Opcode::PUSHGOI => {
                pushgoi(&config);
            }
            Opcode::OR => {
                or(&config);
            }
            Opcode::ORI => {
                ori(&config);
            }
            Opcode::ORN => {
                orn(&config);
            }
            Opcode::ORNI => {
                orni(&config);
            }
            Opcode::NOR => {
                nor(&config);
            }
            Opcode::NORI => {
                nori(&config);
            }
            Opcode::XOR => {
                xor(&config);
            }
            Opcode::XORI => {
                xori(&config);
            }
            Opcode::AND => {
                and(&config);
            }
            Opcode::ANDI => {
                andi(&config);
            }
            Opcode::ANDN => {
                andn(&config);
            }
            Opcode::ANDNI => {
                andni(&config);
            }
            Opcode::NAND => {
                nand(&config);
            }
            Opcode::NANDI => {
                nandi(&config);
            }
            Opcode::NXOR => {
                nxor(&config);
            }
            Opcode::NXORI => {
                nxori(&config);
            }
            Opcode::BDIF => {
                bdif(&config);
            }
            Opcode::BDIFI => {
                bdifi(&config);
            }
            Opcode::WDIF => {
                wdif(&config);
            }
            Opcode::WDIFI => {
                wdifi(&config);
            }
            Opcode::TDIF => {
                tdif(&config);
            }
            Opcode::TDIFI => {
                tdifi(&config);
            }
            Opcode::ODIF => {
                odif(&config);
            }
            Opcode::ODIFI => {
                odifi(&config);
            }
            Opcode::MUX => {
                mux(&config);
            }
            Opcode::MUXI => {
                muxi(&config);
            }
            Opcode::SADD => {
                sadd(&config);
            }
            Opcode::SADDI => {
                saddi(&config);
            }
            Opcode::MOR => {
                mor(&config);
            }
            Opcode::MORI => {
                mori(&config);
            }
            Opcode::MXOR => {
                mxor(&config);
            }
            Opcode::MXORI => {
                mxori(&config);
            }
            Opcode::SETH => {
                seth(&config);
            }
            Opcode::SETMH => {
                setmh(&config);
            }
            Opcode::SETML => {
                setml(&config);
            }
            Opcode::SETL => {
                setl(&config);
            }
            Opcode::INCH => {
                inch(&config);
            }
            Opcode::INCMH => {
                incmh(&config);
            }
            Opcode::INCML => {
                incml(&config);
            }
            Opcode::INCL => {
                incl(&config);
            }
            Opcode::ORH => {
                orh(&config);
            }
            Opcode::ORMH => {
                ormh(&config);
            }
            Opcode::ORML => {
                orml(&config);
            }
            Opcode::ORL => {
                orl(&config);
            }
            Opcode::ANDNH => {
                andnh(&config);
            }
            Opcode::ANDNMH => {
                andnmh(&config);
            }
            Opcode::ANDNML => {
                andnml(&config);
            }
            Opcode::ANDNL => {
                andnl(&config);
            }
            Opcode::JMP => {
                jmp(&config);
            }
            Opcode::JMPB => {
                jmpb(&config);
            }
            Opcode::PUSHJ => {
                pushj(&config);
            }
            Opcode::PUSHJB => {
                pushjb(&config);
            }
            Opcode::GETA => {
                geta(&config);
            }
            Opcode::GETAB => {
                getab(&config);
            }
            Opcode::PUT => {
                put(&config);
            }
            Opcode::PUTI => {
                puti(&config);
            }
            Opcode::POP => {
                pop(&config);
            }
            Opcode::RESUME => {
                resume(&config);
            }
            Opcode::SAVE => {
                save(&config);
            }
            Opcode::UNSAVE => {
                unsave(&config);
            }
            Opcode::SYNC => {
                sync(&config);
            }
            Opcode::SWYM => {
                swym(&config);
            }
            Opcode::GET => {
                get(&config);
            }
            Opcode::TRIP => {
                trip(&config);
            }
        }
    }
    // cleanup
}
