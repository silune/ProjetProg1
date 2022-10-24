(* Bibliothèque pour produire du code X86-64

   2008 Jean-Christophe Filliâtre (CNRS)
   2013 Kim Nguyen (Université Paris Sud)
*)

open Format

type size = [`B | `W | `L | `Q]

type 'size register =  string

let rax = "%rax"
let rbx = "%rbx"
let rcx = "%rcx"
let rdx = "%rdx"
let rsi = "%rsi"
let rdi = "%rdi"
let rbp = "%rbp"
let rsp = "%rsp"
let r8  = "%r8"
let r9  = "%r9"
let r10 = "%r10"
let r11 = "%r11"
let r12 = "%r12"
let r13 = "%r13"
let r14 = "%r14"
let r15 = "%r15"

let eax = "%eax"
let ebx = "%ebx"
let ecx = "%ecx"
let edx = "%edx"
let esi = "%esi"
let edi = "%edi"
let ebp = "%ebp"
let esp = "%esp"
let r8d  = "%r8d"
let r9d  = "%r9d"
let r10d = "%r10d"
let r11d = "%r11d"
let r12d = "%r12d"
let r13d = "%r13d"
let r14d = "%r14d"
let r15d = "%r15d"

let xmm0 = "%xmm0"
let xmm1 = "%xmm1"

let ax = "%ax"
let bx = "%bx"
let cx = "%cx"
let dx = "%dx"
let si = "%si"
let di = "%di"
let bp = "%bp"
let sp = "%sp"
let r8w  = "%r8w"
let r9w  = "%r9w"
let r10w = "%r10w"
let r11w = "%r11w"
let r12w = "%r12w"
let r13w = "%r13w"
let r14w = "%r14w"
let r15w = "%r15w"

let al = "%al"
let bl = "%bl"
let cl = "%cl"
let dl = "%dl"
let ah = "%ah"
let bh = "%bh"
let ch = "%ch"
let dh = "%dh"
let sil = "%sil"
let dil = "%dil"
let bpl = "%bpl"
let spl = "%spl"
let r8b  = "%r8b"
let r9b  = "%r9b"
let r10b = "%r10b"
let r11b = "%r11b"
let r12b = "%r12b"
let r13b = "%r13b"
let r14b = "%r14b"
let r15b = "%r15b"

type label = string

type 'size operand = formatter -> unit -> unit

let mangle_none fmt (l: label) = fprintf fmt "%s" l
let mangle_leading_underscore fmt (l: label) = fprintf fmt "_%s" l
let mangle = mangle_none

let reg r = fun fmt () -> fprintf fmt "%s" r
let (!%) = reg
let imm i = fun fmt () -> fprintf fmt "$%i" i
let imm32 i = fun fmt () -> fprintf fmt "$%ld" i
let imm64 i = fun fmt () -> fprintf fmt "$%Ld" i
let ind ?(ofs=0) ?index ?(scale=1) r = fun fmt () -> match index with
  | None -> fprintf fmt "%d(%s)" ofs r
  | Some r1 -> fprintf fmt "%d(%s,%s,%d)" ofs r r1 scale
let abslab (l: label) = fun fmt () -> fprintf fmt "%a" mangle l
let rellab (l: label) = fun fmt () -> fprintf fmt "%a(%%rip)" mangle l
let lab = abslab
let ilab (l: label) = fun fmt () -> fprintf fmt "$%a" mangle l

type 'a asm =
  | Nop
  | S of string
  | Cat of 'a asm * 'a asm

let nop = Nop
let inline s = S s
let (++) x y = Cat (x, y)

type text = [`text ] asm
type data = [`data ] asm

let buf = Buffer.create 17
let fmt = formatter_of_buffer buf
let ins x =
  Buffer.add_char buf '\t';
  kfprintf (fun fmt ->
    fprintf fmt "\n";
    pp_print_flush fmt ();
    let s = Buffer.contents buf in
    Buffer.clear buf;
    S s
  ) fmt x

let pr_list fmt pr = function
  | []      -> ()
  | [i]     -> pr fmt i
  | i :: ll -> pr fmt i; List.iter (fun i -> fprintf fmt ", %a" pr i) ll

let pr_ilist fmt l =
  pr_list fmt (fun fmt i -> fprintf fmt "%i" i) l

let pr_alist fmt l =
  pr_list fmt (fun fmt (a : label) -> fprintf fmt "%s" a) l

let movb a b = ins "movb \t%a, %a" a () b ()
let movw a b = ins "movw \t%a, %a" a () b ()
let movl a b = ins "movl \t%a, %a" a () b ()
let movq a b = ins "movq \t%a, %a" a () b ()
let movsd a b = ins "movsd \t%a, %a" a () b ()

let movabsq a b = ins "movabsq \t%a, %s" a () b

let movsbw a b = ins "movsbw \t%a, %s" a () b
let movsbl a b = ins "movsbl \t%a, %s" a () b
let movsbq a b = ins "movsbq \t%a, %s" a () b
let movswl a b = ins "movswl \t%a, %s" a () b
let movswq a b = ins "movswq \t%a, %s" a () b
let movslq a b = ins "movslq \t%a, %s" a () b

let movzbw a b = ins "movzbw \t%a, %s" a () b
let movzbl a b = ins "movzbl \t%a, %s" a () b
let movzbq a b = ins "movzbq \t%a, %s" a () b
let movzwl a b = ins "movzwl \t%a, %s" a () b
let movzwq a b = ins "movzwq \t%a, %s" a () b

let leab op r = ins "leab \t%a, %s" op () r
let leaw op r = ins "leaw \t%a, %s" op () r
let leal op r = ins "leal \t%a, %s" op () r
let leaq op r = ins "leaq \t%a, %s" op () r

let incb a = ins "incb \t%a" a ()
let incw a = ins "incw \t%a" a ()
let incl a = ins "incl \t%a" a ()
let incq a = ins "incq \t%a" a ()

let decb a = ins "decb \t%a" a ()
let decw a = ins "decw \t%a" a ()
let decl a = ins "decl \t%a" a ()
let decq a = ins "decq \t%a" a ()

let negb a = ins "negb \t%a" a ()
let negw a = ins "negw \t%a" a ()
let negl a = ins "negl \t%a" a ()
let negq a = ins "negq \t%a" a ()

let addb a b = ins "addb \t%a, %a" a () b ()
let addw a b = ins "addw \t%a, %a" a () b ()
let addl a b = ins "addl \t%a, %a" a () b ()
let addq a b = ins "addq \t%a, %a" a () b ()
let addsd a b = ins "addsd \t%a, %a" a () b ()

let subb a b = ins "subb \t%a, %a" a () b ()
let subw a b = ins "subw \t%a, %a" a () b ()
let subl a b = ins "subl \t%a, %a" a () b ()
let subq a b = ins "subq \t%a, %a" a () b ()

let imulw a b = ins "imulw \t%a, %a" a () b ()
let imull a b = ins "imull \t%a, %a" a () b ()
let imulq a b = ins "imulq \t%a, %a" a () b ()

let idivq a = ins "idivq \t%a" a ()
let cqto = S "\tcqto\n"

let notb a = ins "notb \t%a" a ()
let notw a = ins "notw \t%a" a ()
let notl a = ins "notl \t%a" a ()
let notq a = ins "notq \t%a" a ()

let andb a b = ins "andb \t%a, %a" a () b ()
let andw a b = ins "andw \t%a, %a" a () b ()
let andl a b = ins "andl \t%a, %a" a () b ()
let andq a b = ins "andq \t%a, %a" a () b ()

let orb  a b = ins "orb \t%a, %a" a () b ()
let orw  a b = ins "orw \t%a, %a" a () b ()
let orl  a b = ins "orl \t%a, %a" a () b ()
let orq  a b = ins "orq \t%a, %a" a () b ()

let xorb a b = ins "xorb \t%a, %a" a () b ()
let xorw a b = ins "xorw \t%a, %a" a () b ()
let xorl a b = ins "xorl \t%a, %a" a () b ()
let xorq a b = ins "xorq \t%a, %a" a () b ()

let shlb a b = ins "shlb \t%a, %a" a () b ()
let shlw a b = ins "shlw \t%a, %a" a () b ()
let shll a b = ins "shll \t%a, %a" a () b ()
let shlq a b = ins "shlq \t%a, %a" a () b ()

let shrb a b = ins "shrb \t%a, %a" a () b ()
let shrw a b = ins "shrw \t%a, %a" a () b ()
let shrl a b = ins "shrl \t%a, %a" a () b ()
let shrq a b = ins "shrq \t%a, %a" a () b ()

let sarb a b = ins "sarb \t%a, %a" a () b ()
let sarw a b = ins "sarw \t%a, %a" a () b ()
let sarl a b = ins "sarl \t%a, %a" a () b ()
let sarq a b = ins "sarq \t%a, %a" a () b ()

let jmp (z: label) = ins "jmp \t%s" z
let jmp_star o = ins "jmp \t*%a" o ()

let call (z: label) = ins "call \t%a" mangle z
let call_star z = ins "call \t*%a" z ()
let leave = ins "leave"
let ret = ins "ret"

let je (z: label) = ins "je \t%s" z
let jz (z: label) = ins "jz \t%s" z
let jne(z: label) = ins "jne \t%s" z
let jnz(z: label) = ins "jnz \t%s" z
let js (z: label) = ins "js \t%s" z
let jns(z: label) = ins "jns \t%s" z
let jg (z: label) = ins "jg \t%s" z
let jge(z: label) = ins "jge \t%s" z
let jl (z: label) = ins "jl \t%s" z
let jle(z: label) = ins "jle \t%s" z
let ja (z: label) = ins "ja \t%s" z
let jae(z: label) = ins "jae \t%s" z
let jb (z: label) = ins "jb \t%s" z
let jbe(z: label) = ins "jbe \t%s" z

let cmpb a b = ins "cmpb \t%a, %a" a () b ()
let cmpw a b = ins "cmpw \t%a, %a" a () b ()
let cmpl a b = ins "cmpl \t%a, %a" a () b ()
let cmpq a b = ins "cmpq \t%a, %a" a () b ()

let testb a b = ins "testb \t%a, %a" a () b ()
let testw a b = ins "testw \t%a, %a" a () b ()
let testl a b = ins "testl \t%a, %a" a () b ()
let testq a b = ins "testq \t%a, %a" a () b ()

let sete  a = ins "sete \t%a" a ()
let setne a = ins "setne \t%a" a ()
let sets  a = ins "sets \t%a" a ()
let setns a = ins "setns \t%a" a ()
let setg  a = ins "setg \t%a" a ()
let setge a = ins "setge \t%a" a ()
let setl  a = ins "setl \t%a" a ()
let setle a = ins "setle \t%a" a ()
let seta  a = ins "seta \t%a" a ()
let setae a = ins "setae \t%a" a ()
let setb  a = ins "setb \t%a" a ()
let setbe a = ins "setbe \t%a" a ()

let label (s : label) = S (asprintf "%a:\n" mangle s)
let globl (s: label) = S (asprintf "\t.globl\t%a\n" mangle s)

let comment s = S ("#" ^ s ^ "\n")

let align n = ins ".align %i" n

let dbyte l = ins ".byte %a" pr_ilist l
let dint  l = ins ".int %a" pr_ilist l
let dword l = ins ".word %a" pr_ilist l
let dquad l = ins ".quad %a" pr_ilist l
let string s = ins ".string %S" s

let address l = ins ".quad %a" pr_alist l
let space n = ins ".space %d" n

let pushq a = ins "pushq \t%a" a ()
let popq r = ins "popq \t%s" r

type program = {
  text : [ `text ] asm;
  data : [ `data ] asm;
}

let rec pr_asm fmt = function
  | Nop          -> ()
  | S s          -> fprintf fmt "%s" s
  | Cat (a1, a2) -> pr_asm fmt a1; pr_asm fmt a2

let print_program fmt p =
  fprintf fmt "\t.text\n";
  pr_asm fmt p.text;
  fprintf fmt "\t.data\n";
  pr_asm fmt p.data;
  pp_print_flush fmt ()

let print_in_file ~file p =
  let c = open_out file in
  let fmt = formatter_of_out_channel c in
  print_program fmt p;
  close_out c
