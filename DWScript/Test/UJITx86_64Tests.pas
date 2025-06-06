unit UJITx86_64Tests;

interface

{$IF Defined(WIN64)}

uses
  Classes, SysUtils, Variants,
  dwsXPlatformTests, dwsJITx86Intrinsics, dwsUtils,
  BeaEngineDelphi64, AnsiStrings;

type

  TJITx86_64Tests = class(TTestCase)
  private
    FStream: Tx86_64_WriteOnlyStream;

  protected
    function DisasmStream: String;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure xmm_ops;
    procedure comisd;
    procedure cvt_si2sd_sd2si;
    procedure xor_ops;
    procedure movsd;
    // procedure movq;
    procedure mov_reg_qword_ptr_reg;
    procedure mov_qword_ptr_reg_reg;
    procedure mov_reg_byte_ptr_reg;
    procedure mov_byte_ptr_reg_imm;
    procedure mov_ops;
    procedure mov_reg_reg;
    procedure mov_reg32_reg32;
    procedure mov_reg_imm;
    procedure cmov_reg_reg;
    procedure inc_dec_64;
    // procedure mov_64;
    // procedure add_sub_32;
    // procedure add_sub_execmem;
    // procedure inc_dword_ptr;
    // procedure add_sub_dword_ptr_reg;
    // procedure and_or_xor_dword_ptr_reg;
    procedure neg_not;
    procedure shr_shl_sar_sal;
    // procedure xor_and_or_cmp_32;
    // procedure xor_and_or_cmp_reg;
    // procedure mul_imul_reg;
    procedure imul_qword_ptr_reg;
    procedure idiv_qword_ptr_reg;
    procedure push_pop;
    procedure nops;
    procedure calls;
    procedure cmp_execmem_imm;
    // procedure cmp_dword_ptr_reg_reg;
    procedure cmp_reg_imm;
    procedure ops;
    procedure test_reg_reg;
    procedure test_reg_int32;
    // procedure test_dword_ptr_reg_int32;
    // procedure test_dword_ptr_reg_byte;
    // procedure test_dword_ptr_reg_reg;
    // procedure and_or_byte;
    procedure boolflags;
    procedure movsd_indexed;
    procedure mov_indexed;
    procedure movsx;
    procedure lea;
    procedure vcmp;
    procedure vpcmpeq;
    procedure vblend;
    procedure vbroadcast;
    procedure v_op_pd;
    procedure v_op_ps;
    procedure _movups_ptr_reg;
    procedure _movups_ptr_reg_reg;
    procedure _vmovdqu;
    procedure _vmovapd;
    procedure _vmovupd_ptr_indexed;
    procedure _vmovups_ptr_indexed;
    procedure _vmovupd_ptr_reg;
    procedure _vmovups_ptr_reg;
    procedure _vfma;
    procedure _vpround;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TJITx86_64Tests ------------------
// ------------------

// SetUp
//
procedure TJITx86_64Tests.SetUp;
begin
  FStream := Tx86_64_WriteOnlyStream.Create;
end;

// TearDown
//
procedure TJITx86_64Tests.TearDown;
begin
  FStream.Free;
end;

// DisasmStream
//
function TJITx86_64Tests.DisasmStream: String;
var
  b: TBytes;
  d: TDISASM;
  len: Integer;
  blockEnd: NativeInt;
begin
  Result := '';
  b := FStream.ToBytes;
  FStream.Clear;

  if Length(b) = 0 then
    Exit;

  FillChar(d, SizeOf(d), 0);
  d.EIP := IntPtr(@b[0]);
  blockEnd := d.EIP + Length(b);

  repeat
    len := Disasm(d);
    if len > 0 then
    begin
      FStream.WriteString(UTF8ToString(AnsiStrings.StrPas(d.CompleteInstr)));
      FStream.WriteCRLF;
      d.EIP := d.EIP + len;
      d.SecurityBlock := blockEnd - d.EIP;
    end
    else
      Break;
  until d.SecurityBlock <= 0;
  Result := FStream.ToString;
  FStream.Clear;
end;

// xmm_ops
//
procedure TJITx86_64Tests.xmm_ops;
begin
  FStream._xmm_reg_reg(xmm_addsd, xmm0, xmm1);
  FStream._xmm_reg_reg(xmm_addsd, xmm8, xmm2);
  FStream._xmm_reg_reg(xmm_addsd, xmm3, xmm10);
  FStream._xmm_reg_reg(xmm_addsd, xmm15, xmm11);
  FStream._xmm_reg_reg(xmm_subsd, xmm1, xmm2);
  FStream._xmm_reg_reg(xmm_multsd, xmm2, xmm3);
  FStream._xmm_reg_reg(xmm_divsd, xmm3, xmm4);
  FStream._xmm_reg_reg(xmm_minsd, xmm4, xmm5);
  FStream._xmm_reg_reg(xmm_maxsd, xmm6, xmm7);
  FStream._xmm_reg_reg(xmm_sqrtsd, xmm7, xmm0);
  FStream._xmm_reg_reg(xmm_cvtsi2sd, xmm6, xmm1);

  CheckEquals('addsd xmm0, xmm1'#13#10 + 'addsd xmm8, xmm2'#13#10 +
    'addsd xmm3, xmm10'#13#10 + 'addsd xmm15, xmm11'#13#10 +
    'subsd xmm1, xmm2'#13#10 + 'mulsd xmm2, xmm3'#13#10 +
    'divsd xmm3, xmm4'#13#10 + 'minsd xmm4, xmm5'#13#10 +
    'maxsd xmm6, xmm7'#13#10 + 'sqrtsd xmm7, xmm0'#13#10 +
    'cvtsi2sd xmm6, ecx'#13#10, DisasmStream);

  FStream._xmm_reg_execmem(xmm_addsd, xmm0, $11);
  FStream._xmm_reg_execmem(xmm_addsd, xmm10, $10);
  FStream._xmm_reg_execmem(xmm_multsd, xmm1, $2233);

  // TODO: reactivate pending resolution of https://github.com/BeaEngine/beaengine/issues/33

  // FStream._xmm_reg_absmem(xmm_divsd, xmm2, Pointer($1234));
  // FStream._xmm_reg_absmem(xmm_subsd, xmm8, Pointer($12345));

  CheckEquals('addsd xmm0, qword ptr [' + cgpRegister64Name[cExecMemGPR] +
    '+000001A0h]'#13#10 + 'addsd xmm10, qword ptr [' + cgpRegister64Name
    [cExecMemGPR] + '+00000188h]'#13#10 + 'mulsd xmm1, qword ptr [' +
    cgpRegister64Name[cExecMemGPR] + '+000334D0h]'#13#10
    // +'divsd xmm2, qword ptr [00001234h]'#13#10
    // +'subsd xmm8, qword ptr [00012345h]'#13#10
    , DisasmStream);
end;

// comisd
//
procedure TJITx86_64Tests.comisd;
begin
  FStream._comisd_reg_reg(xmm0, xmm7);
  FStream._comisd_reg_reg(xmm8, xmm1);
  FStream._comisd_reg_reg(xmm9, xmm15);
  FStream._comisd_reg_execmem(xmm1, $11);
  FStream._comisd_reg_execmem(xmm10, $22);
  FStream._comisd_reg_execmem(xmm1, $2233);
  FStream._comisd_reg_absmem(xmm0, Pointer($1234));
  FStream._comisd_reg_absmem(xmm8, Pointer($123456789A));

  CheckEquals('comisd xmm0, xmm7'#13#10 + 'comisd xmm8, xmm1'#13#10 +
    'comisd xmm9, xmm15'#13#10 + 'comisd xmm1, qword ptr [' + cgpRegister64Name
    [cExecMemGPR] + '+000001A0h]'#13#10 + 'comisd xmm10, qword ptr [' +
    cgpRegister64Name[cExecMemGPR] + '+00000338h]'#13#10 +
    'comisd xmm1, qword ptr [' + cgpRegister64Name[cExecMemGPR] +
    '+000334D0h]'#13#10 + 'mov eax, 00001234h'#13#10 +
    'comisd xmm0, qword ptr [rax]'#13#10 + 'mov rax, 000000123456789Ah'#13#10 +
    'comisd xmm8, qword ptr [rax]'#13#10, DisasmStream);
end;

// cvt_si2sd_sd2si
//
procedure TJITx86_64Tests.cvt_si2sd_sd2si;
var
  reg: TgpRegister64;
  xmm: TxmmRegister;
begin
  for reg := gprRAX to gprR15 do
  begin
    for xmm := xmm0 to xmm15 do
    begin
      FStream._cvtsi2sd(xmm, reg);
      FStream._xmm_reg_qword_ptr_reg(xmm_cvtsi2sd, xmm, reg, $12);
      FStream._cvtsd2si(reg, xmm);
      FStream._cvttsd2si(reg, xmm);
      CheckEquals('cvtsi2sd xmm' + IntToStr(Ord(xmm)) + ', ' + cgpRegister64Name
        [reg] + #13#10 + 'cvtsi2sd xmm' + IntToStr(Ord(xmm)) + ', dword ptr [' +
        cgpRegister64Name[reg] + '+12h]'#13#10 + 'cvtsd2si ' + cgpRegister64Name
        [reg] + ', xmm' + IntToStr(Ord(xmm)) + #13#10 + 'cvttsd2si ' +
        cgpRegister64Name[reg] + ', xmm' + IntToStr(Ord(xmm)) + #13#10,
        DisasmStream);
    end;
  end;
end;

// xor_ops
//
procedure TJITx86_64Tests.xor_ops;
begin
  FStream._xorps_reg_reg(xmm1, xmm6);
  FStream._xor_reg_reg(gprRAX, gprRDI);
  FStream._xor_reg_reg(gprRSI, gprRCX);

  CheckEquals('xorps xmm1, xmm6'#13#10 + 'xor rax, rdi'#13#10 +
    'xor rsi, rcx'#13#10, DisasmStream);
end;

// movsd
//
procedure TJITx86_64Tests.movsd;
begin
  FStream._movsd_reg_execmem(xmm3, $11);
  FStream._movsd_reg_execmem(xmm9, $1122);
  FStream._movsd_execmem_reg($22, xmm4);
  FStream._movsd_execmem_reg($2244, xmm12);

  // TODO: reactivate pending resolution of https://github.com/BeaEngine/beaengine/issues/33

  FStream._movsd_reg_absmem(xmm0, Pointer($12345));
  FStream._movsd_reg_absmem(xmm10, Pointer($123456789A));

  CheckEquals('' + 'movsd xmm3, qword ptr [' + cgpRegister64Name[cExecMemGPR] +
    '+000001A0h]'#13#10 + 'movsd xmm9, qword ptr [' + cgpRegister64Name
    [cExecMemGPR] + '+00019B38h]'#13#10 + 'movsd qword ptr [' +
    cgpRegister64Name[cExecMemGPR] + '+00000338h], xmm4'#13#10 +
    'movsd qword ptr [' + cgpRegister64Name[cExecMemGPR] +
    '+00033668h], xmm12'#13#10 + 'mov eax, 00012345h'#13#10 +
    'movsd xmm0, qword ptr [rax]'#13#10 + 'mov rax, 000000123456789Ah'#13#10 +
    'movsd xmm10, qword ptr [rax]'#13#10, DisasmStream);

  // FStream._movsd_reg_esp(xmm1);
  // FStream._movsd_reg_esp(xmm11);
  // FStream._movsd_esp_reg(xmm2);
  // FStream._movsd_esp_reg(xmm8);
  FStream._movsd_qword_ptr_reg_reg(gprRDX, $11, xmm3);
  FStream._movsd_qword_ptr_reg_reg(gprRCX, $1122, xmm8);
  FStream._movsd_qword_ptr_reg_reg(gprR8, $22, xmm7);

  FStream._movsd_reg_qword_ptr_reg(xmm3, gprRAX, $22);
  FStream._movsd_reg_qword_ptr_reg(xmm9, gprRCX, $80);
  FStream._movsd_reg_qword_ptr_reg(xmm1, gprR11, -$10);

  CheckEquals(''
    // +'movsd xmm1, qword ptr [esp]'#13#10
    // +'movsd qword ptr [esp], xmm2'#13#10
    + 'movsd qword ptr [rdx+11h], xmm3'#13#10 +
    'movsd qword ptr [rcx+00001122h], xmm8'#13#10 +
    'movsd qword ptr [r8+22h], xmm7'#13#10 +
    'movsd xmm3, qword ptr [rax+22h]'#13#10 +
    'movsd xmm9, qword ptr [rcx+00000080h]'#13#10 +
    'movsd xmm1, qword ptr [r11-10h]'#13#10, DisasmStream);
end;

// movq
//
// procedure TJITx86_64Tests.movq;
// begin
// FStream._movq_execmem_reg($22, xmm4);
// FStream._movq_execmem_reg($2244, xmm8);
// FStream._movq_reg_absmem(xmm3, Pointer($11));
// FStream._movq_reg_absmem(xmm15, Pointer($22));
//
// CheckEquals( 'movq qword ptr ['+cgpRegister64Name[cExecMemGPR]+'+00000228h], xmm4'#13#10
// +'movq qword ptr ['+cgpRegister64Name[cExecMemGPR]+'+00022448h], xmm8'#13#10
// +'movq xmm3, qword ptr [00000011h]'#13#10
// +'movq xmm15, qword ptr [00000022h]'#13#10
// , DisasmStream);
// end;

// mov_reg_qword_ptr_reg
//
procedure TJITx86_64Tests.mov_reg_qword_ptr_reg;
var
  offset: Integer;
  dest, src: TgpRegister64;
  expect: String;
begin
  for dest := gprRAX to gprR15 do
  begin
    for src := gprRAX to gprR15 do
    begin
      expect := '';
      for offset := 0 to 2 do
      begin
        FStream._mov_reg_qword_ptr_reg(dest, src, offset * $40);
        expect := expect + 'mov ' + cgpRegister64Name[dest] + ', qword ptr [' +
          cgpRegister64Name[src];
        case offset of
          1:
            expect := expect + '+40h';
          2:
            expect := expect + '+00000080h';
        else
          if src in [gprRBP, gprRSP, gprR12, gprR13] then
            expect := expect + '+00h';
        end;
        expect := expect + ']'#13#10;
      end;
      CheckEquals(expect, DisasmStream);
    end;
  end;
end;

// mov_reg_byte_ptr_reg
//
procedure TJITx86_64Tests.mov_reg_byte_ptr_reg;
var
  offset: Integer;
  dest, src: TgpRegister64;
  expect: String;
begin
  for dest := gprRAX to gprR15 do
  begin
    for src := gprRAX to gprR15 do
    begin
      expect := '';
      for offset := 0 to 2 do
      begin
        FStream._mov_reg_byte_ptr_reg(dest, src, offset * $40);
        expect := expect + 'movzx ' + cgpRegister64Name[dest] + ', byte ptr [' +
          cgpRegister64Name[src];
        case offset of
          1:
            expect := expect + '+40h';
          2:
            expect := expect + '+00000080h';
        else
          if src in [gprRBP, gprRSP, gprR12, gprR13] then
            expect := expect + '+00h';
        end;
        expect := expect + ']'#13#10;
      end;
      CheckEquals(expect, DisasmStream);
    end;
  end;
end;

// mov_byte_ptr_reg_imm
//
procedure TJITx86_64Tests.mov_byte_ptr_reg_imm;
var
  offset: Integer;
  dest: TgpRegister64;
  expect: String;
begin
  for dest := gprRAX to gprR15 do
  begin
    expect := '';
    for offset := 0 to 2 do
    begin
      FStream._mov_byte_ptr_reg_imm(dest, offset * $40, $12);
      expect := expect + 'mov byte ptr [' + cgpRegister64Name[dest];
      case offset of
        1:
          expect := expect + '+40h';
        2:
          expect := expect + '+00000080h';
      else
        if dest in [gprRBP, gprRSP, gprR12, gprR13] then
          expect := expect + '+00h';
      end;
      expect := expect + '], 12h'#13#10;
    end;
    CheckEquals(expect, DisasmStream);
  end;
end;

// mov_qword_ptr_reg_reg
//
procedure TJITx86_64Tests.mov_qword_ptr_reg_reg;
var
  offset: Integer;
  dest, src: TgpRegister64;
  expect: String;
begin
  for dest := gprRAX to gprR15 do
  begin
    for src := gprRAX to gprR15 do
    begin
      expect := '';
      for offset := 0 to 2 do
      begin
        FStream._mov_qword_ptr_reg_reg(dest, offset * $40, src);
        expect := expect + 'mov qword ptr [' + cgpRegister64Name[dest];
        case offset of
          1:
            expect := expect + '+40h';
          2:
            expect := expect + '+00000080h';
        else
          if dest in [gprRBP, gprRSP, gprR12, gprR13] then
            expect := expect + '+00h';
        end;
        expect := expect + '], ' + cgpRegister64Name[src] + #13#10;
      end;
      CheckEquals(expect, DisasmStream);
    end;
  end;
end;

// mov_ops
//
procedure TJITx86_64Tests.mov_ops;
begin
  FStream._mov_reg_execmem(gprRAX, $1);
  FStream._mov_reg_execmem(gprRDX, $100, 1);
  FStream._mov_execmem_reg($2, gprRCX);
  FStream._mov_execmem_reg($2233, gprRCX);

  CheckEquals('mov rax, qword ptr [' + cgpRegister64Name[cExecMemGPR] +
    '+20h]'#13#10 + 'mov rdx, qword ptr [' + cgpRegister64Name[cExecMemGPR] +
    '+00001809h]'#13#10 + 'mov qword ptr [' + cgpRegister64Name[cExecMemGPR] +
    '+38h], rcx'#13#10 + 'mov qword ptr [' + cgpRegister64Name[cExecMemGPR] +
    '+000334D0h], rcx'#13#10, DisasmStream);

  FStream._mov_execmem_imm(0, 0);
  FStream._mov_execmem_imm(1, 1);
  FStream._mov_execmem_imm($100, 0);
  FStream._mov_execmem_imm($100, -1);
  FStream._mov_execmem_imm($100, $11);

  CheckEquals('' + 'mov qword ptr [' + cgpRegister64Name[cExecMemGPR] +
    '+08h], 0000000000000000h'#13#10 + 'mov qword ptr [' + cgpRegister64Name
    [cExecMemGPR] + '+20h], 0000000000000001h'#13#10 + 'mov qword ptr [' +
    cgpRegister64Name[cExecMemGPR] + '+00001808h], 0000000000000000h'#13#10 +
    'mov qword ptr [' + cgpRegister64Name[cExecMemGPR] +
    '+00001808h], FFFFFFFFFFFFFFFFh'#13#10 + 'mov qword ptr [' +
    cgpRegister64Name[cExecMemGPR] + '+00001808h], 0000000000000011h'#13#10,
    DisasmStream);

  FStream._mov_reg_imm(gprRAX, 0);
  FStream._mov_reg_imm(gprRAX, 0, True);
  FStream._mov_reg_imm(gprRDX, 0);
  FStream._mov_reg_imm(gprR9, 0);
  FStream._mov_reg_imm(gprRAX, 1);
  FStream._mov_reg_imm(gprRAX, -1);
  FStream._mov_reg_imm(gprR8, 2);
  FStream._mov_reg_imm(gprR8, -2);
  FStream._mov_reg_imm(gprRAX, 3);
  FStream._mov_reg_imm(gprRAX, -3);
  FStream._mov_reg_imm(gprRAX, $1122334455);
  FStream._mov_reg_imm(gprRAX, $11223344556677);
  FStream._mov_reg_imm(gprR11, $11223344556677);

  CheckEquals('xor rax, rax'#13#10 + 'mov eax, 00000000h'#13#10 +
    'xor rdx, rdx'#13#10 + 'xor r9, r9'#13#10 + 'mov eax, 00000001h'#13#10 +
    'mov rax, FFFFFFFFFFFFFFFFh'#13#10 + 'mov r8d, 00000002h'#13#10 +
    'mov r8, FFFFFFFFFFFFFFFEh'#13#10 + 'mov eax, 00000003h'#13#10 +
    'mov rax, FFFFFFFFFFFFFFFDh'#13#10 + 'mov rax, 0000001122334455h'#13#10 +
    'mov rax, 0011223344556677h'#13#10 + 'mov r11, 0011223344556677h'#13#10,
    DisasmStream);
end;

// mov_reg_reg
//
procedure TJITx86_64Tests.mov_reg_reg;
var
  dest, src: TgpRegister64;
  expect: String;
begin
  for dest := gprRAX to gprR15 do
  begin
    expect := '';
    for src := gprRAX to gprR15 do
    begin
      FStream._mov_reg_reg(dest, src);
      if dest <> src then
        expect := expect + 'mov ' + cgpRegister64Name[dest] + ', ' +
          cgpRegister64Name[src] + #13#10;
    end;
    CheckEquals(expect, DisasmStream);
  end;
end;

// mov_reg32_reg32
//
procedure TJITx86_64Tests.mov_reg32_reg32;
var
  dest, src: TgpRegister64;
  expect: String;
begin
  for dest := gprRAX to gprR15 do
  begin
    expect := '';
    for src := gprRAX to gprR15 do
    begin
      FStream._mov_reg32_reg32(dest, src);
      expect := expect + 'mov ' + cgpRegister64dName[dest] + ', ' +
        cgpRegister64dName[src] + #13#10;
    end;
    CheckEquals(expect, DisasmStream);
  end;
end;

// mov_reg_imm
//
procedure TJITx86_64Tests.mov_reg_imm;
var
  dest: TgpRegister64;
  expect, regName, reg32name: String;
begin
  for dest := gprRAX to gprR15 do
  begin
    FStream._mov_reg_dword(dest, 0);
    FStream._mov_reg_qword(dest, 1);
    FStream._mov_reg_imm(dest, $80);
    FStream._mov_reg_imm(dest, -1);
    FStream._mov_reg_imm(dest, -2);
    regName := cgpRegister64Name[dest];
    reg32name := cgpRegister64dName[dest];
    expect := 'xor ' + regName + ', ' + regName + #13#10 + 'mov ' + reg32name +
      ', 00000001h'#13#10 + 'mov ' + reg32name + ', 00000080h'#13#10 + 'mov ' +
      regName + ', FFFFFFFFFFFFFFFFh'#13#10 + 'mov ' + regName +
      ', FFFFFFFFFFFFFFFEh'#13#10;
    CheckEquals(expect, DisasmStream);
  end;
end;

// cmov_reg_reg
//
procedure TJITx86_64Tests.cmov_reg_reg;
begin
  FStream._cmov(flagsNBE, gprRAX, gprRBP);
  FStream._cmov(flagsB, gprRCX, gprR10);
  FStream._cmov(flagsE, gprR8, gprR11);
  FStream._cmov(flagsNZ, gprR9, gprRDX);
  CheckEquals('' + 'cmovnbe rax, rbp'#13#10 + 'cmovb rcx, r10'#13#10 +
    'cmove r8, r11'#13#10 + 'cmovne r9, rdx'#13#10, DisasmStream);
end;

// inc_dec_64
//
procedure TJITx86_64Tests.inc_dec_64;
begin
  FStream._add_reg_imm(gprRAX, 0);
  FStream._add_reg_imm(gprRAX, 1);
  FStream._add_reg_imm(gprR8, 1);
  FStream._add_reg_imm(gprR9, -1);
  FStream._add_reg_imm(gprRCX, $1234);
  FStream._add_reg_imm(gprRDX, $200030001);
  FStream._add_reg_imm(gprR9, $112200000033);

  CheckEquals('' + 'inc rax'#13#10 + 'inc r8'#13#10 + 'dec r9'#13#10 +
    'add rcx, 0000000000001234h'#13#10 + 'mov rax, 0000000200030001h'#13#10 +
    'add rdx, rax'#13#10 + 'mov rax, 0000112200000033h'#13#10 +
    'add r9, rax'#13#10, DisasmStream);

  FStream._add_execmem_imm(0, 1);
  FStream._add_execmem_imm(1, $80);
  FStream._add_execmem_imm($80, $200030001);

  CheckEquals('' + 'inc qword ptr [' + cgpRegister64Name[cExecMemGPR] +
    '+08h]'#13#10 + 'mov eax, 00000080h'#13#10 + 'add qword ptr [' +
    cgpRegister64Name[cExecMemGPR] + '+20h], rax'#13#10 +
    'mov rax, 0000000200030001h'#13#10 + 'add qword ptr [' + cgpRegister64Name
    [cExecMemGPR] + '+00000C08h], rax'#13#10, DisasmStream);

  FStream._sub_reg_imm(gprRAX, 0);
  FStream._sub_reg_imm(gprRAX, 1);
  FStream._sub_reg_imm(gprR8, 1);
  FStream._sub_reg_imm(gprR9, -1);
  FStream._sub_reg_imm(gprRCX, $1234);
  FStream._sub_reg_imm(gprRDX, $200030001);
  FStream._sub_reg_imm(gprR9, $112200000033);

  CheckEquals('' + 'dec rax'#13#10 + 'dec r8'#13#10 + 'inc r9'#13#10 +
    'sub rcx, 0000000000001234h'#13#10 + 'mov rax, 0000000200030001h'#13#10 +
    'sub rdx, rax'#13#10 + 'mov rax, 0000112200000033h'#13#10 +
    'sub r9, rax'#13#10, DisasmStream);

  FStream._sub_execmem_imm(0, 1);
  FStream._sub_execmem_imm(1, $80);
  FStream._sub_execmem_imm($80, $200030001);

  CheckEquals('' + 'dec qword ptr [' + cgpRegister64Name[cExecMemGPR] +
    '+08h]'#13#10 + 'mov eax, 00000080h'#13#10 + 'sub qword ptr [' +
    cgpRegister64Name[cExecMemGPR] + '+20h], rax'#13#10 +
    'mov rax, 0000000200030001h'#13#10 + 'sub qword ptr [' + cgpRegister64Name
    [cExecMemGPR] + '+00000C08h], rax'#13#10, DisasmStream);
end;

{
  // mov_64
  //
  procedure TJITx86_64Tests.mov_64;
  var
  offset : Integer;
  reg : TgpRegister;
  expect, offtextA, offtextD : String;
  begin
  for reg:=gprEAX to gprEDI do begin
  expect:='';
  for offset:=0 to 2 do begin
  FStream._mov_eaxedx_qword_ptr_reg(reg, offset*$40);
  FStream._mov_qword_ptr_reg_eaxedx(reg, offset*$40);
  if offset=0 then begin
  if reg=gprEBP then
  offtextA:='+00h'
  else offtextA:='';
  offtextD:='+04h';
  end else if offset=1 then begin
  offtextA:='+40h';
  offtextD:='+44h';
  end else begin
  offtextA:='+00000080h';
  offtextD:='+00000084h';
  end;
  if reg<>gprEAX then begin
  expect:= expect
  +'mov eax, dword ptr ['+cgpRegisterName[reg]+offtextA+']'#13#10
  +'mov edx, dword ptr ['+cgpRegisterName[reg]+offtextD+']'#13#10;
  end else begin
  expect:= expect
  +'mov edx, dword ptr ['+cgpRegisterName[reg]+offtextD+']'#13#10
  +'mov eax, dword ptr ['+cgpRegisterName[reg]+offtextA+']'#13#10;
  end;
  expect:=expect
  +'mov dword ptr ['+cgpRegisterName[reg]+offtextA+'], eax'#13#10
  +'mov dword ptr ['+cgpRegisterName[reg]+offtextD+'], edx'#13#10
  ;
  end;
  CheckEquals(expect, DisasmStream);
  end;
  end;

  // add_sub_32
  //
  procedure TJITx86_64Tests.add_sub_32;
  var
  reg : TgpRegister;
  expect : String;
  begin
  for reg:=gprEAX to gprEDI do begin
  FStream._add_reg_int32(reg, 1);
  FStream._add_reg_int32(reg, $80);
  FStream._adc_reg_int32(reg, 1);
  FStream._adc_reg_int32(reg, $80);
  FStream._sub_reg_int32(reg, 1);
  FStream._sub_reg_int32(reg, $80);
  FStream._sbb_reg_int32(reg, 1);
  FStream._sbb_reg_int32(reg, $80);
  expect:= 'add '+cgpRegisterName[reg]+', 01h'#13#10
  +'add '+cgpRegisterName[reg]+', 00000080h'#13#10
  +'adc '+cgpRegisterName[reg]+', 01h'#13#10
  +'adc '+cgpRegisterName[reg]+', 00000080h'#13#10
  +'sub '+cgpRegisterName[reg]+', 01h'#13#10
  +'sub '+cgpRegisterName[reg]+', 00000080h'#13#10
  +'sbb '+cgpRegisterName[reg]+', 01h'#13#10
  +'sbb '+cgpRegisterName[reg]+', 00000080h'#13#10
  ;
  CheckEquals(expect, DisasmStream);
  end;
  end;

  // add_sub_execmem
  //
  procedure TJITx86_64Tests.add_sub_execmem;
  var
  reg : TgpRegister;
  expect : String;
  begin
  for reg:=gprEAX to gprEDI do begin
  FStream._add_reg_execmem(reg, 1, 0);
  FStream._add_reg_execmem(reg, $80, 0);
  FStream._adc_reg_execmem(reg, 1, 0);
  FStream._adc_reg_execmem(reg, $80, 0);
  FStream._sub_reg_execmem(reg, 1, 0);
  FStream._sub_reg_execmem(reg, $80, 0);
  FStream._sbb_reg_execmem(reg, 1, 0);
  FStream._sbb_reg_execmem(reg, $80, 0);
  expect:= 'add '+cgpRegisterName[reg]+', dword ptr [ebx+18h]'#13#10
  +'add '+cgpRegisterName[reg]+', dword ptr [ebx+00000808h]'#13#10
  +'adc '+cgpRegisterName[reg]+', dword ptr [ebx+18h]'#13#10
  +'adc '+cgpRegisterName[reg]+', dword ptr [ebx+00000808h]'#13#10
  +'sub '+cgpRegisterName[reg]+', dword ptr [ebx+18h]'#13#10
  +'sub '+cgpRegisterName[reg]+', dword ptr [ebx+00000808h]'#13#10
  +'sbb '+cgpRegisterName[reg]+', dword ptr [ebx+18h]'#13#10
  +'sbb '+cgpRegisterName[reg]+', dword ptr [ebx+00000808h]'#13#10
  ;
  CheckEquals(expect, DisasmStream);
  end;
  for reg:=gprEAX to gprEDI do begin
  FStream._add_execmem_reg(1, 0, reg);
  FStream._add_execmem_reg($80, 0, reg);
  FStream._adc_execmem_reg(1, 0, reg);
  FStream._adc_execmem_reg($80, 0, reg);
  FStream._sub_execmem_reg(1, 0, reg);
  FStream._sub_execmem_reg($80, 0, reg);
  FStream._sbb_execmem_reg(1, 0, reg);
  FStream._sbb_execmem_reg($80, 0, reg);
  expect:= 'add dword ptr [ebx+18h], '+cgpRegisterName[reg]+#13#10
  +'add dword ptr [ebx+00000808h], '+cgpRegisterName[reg]+#13#10
  +'adc dword ptr [ebx+18h], '+cgpRegisterName[reg]+#13#10
  +'adc dword ptr [ebx+00000808h], '+cgpRegisterName[reg]+#13#10
  +'sub dword ptr [ebx+18h], '+cgpRegisterName[reg]+#13#10
  +'sub dword ptr [ebx+00000808h], '+cgpRegisterName[reg]+#13#10
  +'sbb dword ptr [ebx+18h], '+cgpRegisterName[reg]+#13#10
  +'sbb dword ptr [ebx+00000808h], '+cgpRegisterName[reg]+#13#10
  ;
  CheckEquals(expect, DisasmStream);
  end;
  end;

  // inc_dword_ptr
  //
  procedure TJITx86_64Tests.inc_dword_ptr;
  var
  reg : TgpRegister;
  expect : String;
  begin
  for reg:=gprEAX to gprEDI do begin
  FStream._inc_dword_ptr_reg(reg, 0);
  FStream._inc_dword_ptr_reg(reg, $40);
  FStream._inc_dword_ptr_reg(reg, $80);
  if reg=gprEBP then
  expect:='inc dword ptr ['+cgpRegisterName[reg]+'+00h]'#13#10
  else expect:='inc dword ptr ['+cgpRegisterName[reg]+']'#13#10;
  expect:= expect
  +'inc dword ptr ['+cgpRegisterName[reg]+'+40h]'#13#10
  +'inc dword ptr ['+cgpRegisterName[reg]+'+00000080h]'#13#10
  ;
  CheckEquals(expect, DisasmStream);
  end;
  end;

  // add_sub_dword_ptr_reg
  //
  procedure TJITx86_64Tests.add_sub_dword_ptr_reg;
  var
  dest, src : TgpRegister;
  offset : Integer;
  expect, offtextA : String;
  begin
  for dest:=gprEAX to gprEDI do begin
  for src:=gprEAX to gprEDI do begin
  expect:='';
  for offset:=0 to 2 do begin
  FStream._add_reg_dword_ptr_reg(dest, src, offset*$40);
  FStream._adc_reg_dword_ptr_reg(dest, src, offset*$40);
  FStream._sub_reg_dword_ptr_reg(dest, src, offset*$40);
  FStream._sbb_reg_dword_ptr_reg(dest, src, offset*$40);
  if offset=0 then begin
  if src=gprEBP then
  offtextA:='+00h'
  else offtextA:='';
  end else if offset=1 then begin
  offtextA:='+40h';
  end else begin
  offtextA:='+00000080h';
  end;
  expect:= expect
  +'add '+cgpRegisterName[dest]+', dword ptr ['+cgpRegisterName[src]+offtextA+']'#13#10
  +'adc '+cgpRegisterName[dest]+', dword ptr ['+cgpRegisterName[src]+offtextA+']'#13#10
  +'sub '+cgpRegisterName[dest]+', dword ptr ['+cgpRegisterName[src]+offtextA+']'#13#10
  +'sbb '+cgpRegisterName[dest]+', dword ptr ['+cgpRegisterName[src]+offtextA+']'#13#10
  ;
  end;
  CheckEquals(expect, DisasmStream);
  end;
  end;
  end;

  // and_or_xor_dword_ptr_reg
  //
  procedure TJITx86_64Tests.and_or_xor_dword_ptr_reg;
  var
  dest, src : TgpRegister;
  offset : Integer;
  expect, offtextA : String;
  begin
  for dest:=gprEAX to gprEDI do begin
  for src:=gprEAX to gprEDI do begin
  expect:='';
  for offset:=0 to 2 do begin
  FStream._and_reg_dword_ptr_reg(dest, src, offset*$40);
  FStream._or_reg_dword_ptr_reg(dest, src, offset*$40);
  FStream._xor_reg_dword_ptr_reg(dest, src, offset*$40);
  if offset=0 then begin
  if src=gprEBP then
  offtextA:='+00h'
  else offtextA:='';
  end else if offset=1 then begin
  offtextA:='+40h';
  end else begin
  offtextA:='+00000080h';
  end;
  expect:= expect
  +'and '+cgpRegisterName[dest]+', dword ptr ['+cgpRegisterName[src]+offtextA+']'#13#10
  +'or '+cgpRegisterName[dest]+', dword ptr ['+cgpRegisterName[src]+offtextA+']'#13#10
  +'xor '+cgpRegisterName[dest]+', dword ptr ['+cgpRegisterName[src]+offtextA+']'#13#10
  ;
  end;
  CheckEquals(expect, DisasmStream);
  end;
  end;
  end;
}
// neg_not
//
procedure TJITx86_64Tests.neg_not;
var
  reg: TgpRegister64;
  expect: String;
begin
  for reg := gprRAX to gprR15 do
  begin
    FStream._neg_reg(reg);
    FStream._not_reg(reg);
    expect := 'neg ' + cgpRegister64Name[reg] + #13#10 + 'not ' +
      cgpRegister64Name[reg] + #13#10;
    CheckEquals(expect, DisasmStream);
  end;
end;

// shr_shl_sar_sal
//
procedure TJITx86_64Tests.shr_shl_sar_sal;
var
  reg: TgpRegister64;
  expect: String;
begin
  for reg := gprRAX to gprR15 do
  begin
    FStream._shift_reg_imm(gpShr, reg, 1);
    FStream._shift_reg_imm(gpShr, reg, $7F);
    FStream._shift_reg_imm(gpShl, reg, 1);
    FStream._shift_reg_imm(gpShl, reg, $7F);
    FStream._shift_reg_imm(gpSar, reg, 1);
    FStream._shift_reg_imm(gpSar, reg, $7F);
    FStream._shift_reg_imm(gpSal, reg, 1);
    FStream._shift_reg_imm(gpSal, reg, $7F);
    expect := 'shr ' + cgpRegister64Name[reg] + ', 1'#13#10 + 'shr ' +
      cgpRegister64Name[reg] + ', 7Fh'#13#10 + 'shl ' + cgpRegister64Name[reg] +
      ', 1'#13#10 + 'shl ' + cgpRegister64Name[reg] + ', 7Fh'#13#10 + 'sar ' +
      cgpRegister64Name[reg] + ', 1'#13#10 + 'sar ' + cgpRegister64Name[reg] +
      ', 7Fh'#13#10 + 'sal ' + cgpRegister64Name[reg] + ', 1'#13#10 + 'sal ' +
      cgpRegister64Name[reg] + ', 7Fh'#13#10;
    CheckEquals(expect, DisasmStream);
  end;
end;

{
  // xor_and_or_32
  //
  procedure TJITx86_64Tests.xor_and_or_cmp_32;
  var
  reg : TgpRegister;
  expect : String;
  begin
  for reg:=gprEAX to gprEDI do begin
  FStream._op_reg_int32(gpOp_xor, reg, 1);
  FStream._op_reg_int32(gpOp_xor,reg, $80);
  FStream._op_reg_int32(gpOp_and, reg, 1);
  FStream._op_reg_int32(gpOp_and,reg, $80);
  FStream._op_reg_int32(gpOp_or, reg, 1);
  FStream._op_reg_int32(gpOp_or,reg, $80);
  FStream._op_reg_int32(gpOp_cmp, reg, 1);
  FStream._op_reg_int32(gpOp_cmp,reg, $80);
  expect:= 'xor '+cgpRegisterName[reg]+', 01h'#13#10
  +'xor '+cgpRegisterName[reg]+', 00000080h'#13#10
  +'and '+cgpRegisterName[reg]+', 01h'#13#10
  +'and '+cgpRegisterName[reg]+', 00000080h'#13#10
  +'or '+cgpRegisterName[reg]+', 01h'#13#10
  +'or '+cgpRegisterName[reg]+', 00000080h'#13#10
  +'cmp '+cgpRegisterName[reg]+', 01h'#13#10
  +'cmp '+cgpRegisterName[reg]+', 00000080h'#13#10
  ;
  CheckEquals(expect, DisasmStream);
  end;
  end;

  // xor_and_or_cmp_reg
  //
  procedure TJITx86_64Tests.xor_and_or_cmp_reg;
  var
  dest, src : TgpRegister;
  expect : String;
  begin
  for dest:=gprEAX to gprEDI do begin
  for src:=gprEAX to gprEDI do begin
  FStream._op_reg_reg(gpOp_xor, dest, src);
  FStream._op_reg_reg(gpOp_and, dest, src);
  FStream._op_reg_reg(gpOp_or, dest, src);
  FStream._op_reg_reg(gpOp_cmp, dest, src);
  FStream._op_reg_reg(gpOp_add, dest, src);
  FStream._op_reg_reg(gpOp_adc, dest, src);
  FStream._op_reg_reg(gpOp_sub, dest, src);
  FStream._op_reg_reg(gpOp_sbb, dest, src);
  expect:= 'xor '+cgpRegisterName[dest]+', '+cgpRegisterName[src]+#13#10
  +'and '+cgpRegisterName[dest]+', '+cgpRegisterName[src]+#13#10
  +'or '+cgpRegisterName[dest]+', '+cgpRegisterName[src]+#13#10
  +'cmp '+cgpRegisterName[dest]+', '+cgpRegisterName[src]+#13#10
  +'add '+cgpRegisterName[dest]+', '+cgpRegisterName[src]+#13#10
  +'adc '+cgpRegisterName[dest]+', '+cgpRegisterName[src]+#13#10
  +'sub '+cgpRegisterName[dest]+', '+cgpRegisterName[src]+#13#10
  +'sbb '+cgpRegisterName[dest]+', '+cgpRegisterName[src]+#13#10
  ;
  CheckEquals(expect, DisasmStream);
  end;
  end;
  end;

  // mul_imul_reg
  //
  procedure TJITx86_64Tests.mul_imul_reg;
  var
  dest, src : TgpRegister;
  expect : String;
  begin
  for dest:=gprEAX to gprEDI do begin
  FStream._mul_reg(dest);
  expect:='mul '+cgpRegisterName[dest]+#13#10;
  for src:=gprEAX to gprEDI do begin
  FStream._imul_reg_reg(dest, src);
  expect:= expect+'imul '+cgpRegisterName[dest]+', '+cgpRegisterName[src]+#13#10
  ;
  end;
  CheckEquals(expect, DisasmStream);
  end;
  end;
}
// imul_qword_ptr_reg
//
procedure TJITx86_64Tests.imul_qword_ptr_reg;
var
  reg, src: TgpRegister64;
  offset: Integer;
  expect, offsetText: String;
begin
  for offset := 0 to 2 do
  begin
    for src := gprRAX to gprR15 do
    begin
      case offset of
        1:
          offsetText := '+40h';
        2:
          offsetText := '+00000080h';
      else
        if src in [gprRBP, gprRSP, gprR12, gprR13] then
          offsetText := '+00h'
        else
          offsetText := '';
      end;
      // expect:='mul qword ptr ['+cgpRegisterName[src]+offsetText+']'#13#10;
      // FStream._mul_qword_ptr_reg(src, offset*$40);
      expect := '';
      for reg := gprRAX to gprR15 do
      begin
        FStream._imul_reg_qword_ptr_reg(reg, src, offset * $40);
        expect := expect + 'imul ' + cgpRegister64Name[reg] + ', qword ptr [' +
          cgpRegister64Name[src] + offsetText + ']'#13#10;
      end;
      CheckEquals(expect, DisasmStream);
    end;
  end;
end;

// idiv_qword_ptr_reg
//
procedure TJITx86_64Tests.idiv_qword_ptr_reg;
var
  reg: TgpRegister64;
  offset: Integer;
  expect, offsetText: String;
begin
  for offset := 0 to 2 do
  begin
    for reg := gprRAX to gprR15 do
    begin
      case offset of
        1:
          offsetText := '+40h';
        2:
          offsetText := '+00000080h';
      else
        if reg in [gprRBP, gprRSP, gprR12, gprR13] then
          offsetText := '+00h'
        else
          offsetText := '';
      end;
      // expect:='mul qword ptr ['+cgpRegisterName[src]+offsetText+']'#13#10;
      // FStream._mul_qword_ptr_reg(src, offset*$40);
      expect := '';
      FStream._idiv_qword_ptr_reg(reg, offset * $40);
      FStream._idiv_reg(reg);
      expect := expect + 'idiv qword ptr [' + cgpRegister64Name[reg] +
        offsetText + ']'#13#10 + 'idiv ' + cgpRegister64Name[reg] + #13#10;
      CheckEquals(expect, DisasmStream);
    end;
  end;
end;

// push_pop
//
procedure TJITx86_64Tests.push_pop;
var
  dest: TgpRegister64;
  expect: String;
begin
  for dest := gprRAX to gprR15 do
  begin
    FStream._push_reg(dest);
    FStream._pop_reg(dest);
    expect := 'push ' + cgpRegister64Name[dest] + #13#10 + 'pop ' +
      cgpRegister64Name[dest] + #13#10;
    CheckEquals(expect, DisasmStream);
  end;
end;

// nops
//
procedure TJITx86_64Tests.nops;
var
  i: Integer;
  buf: String;
begin
  for i := 0 to 15 do
  begin
    FStream._nop(i);
    FStream._ret;
  end;

  buf := DisasmStream;
  buf := StringReplace(buf, 'nop dword ptr [rax]'#13#10, 'n', [rfReplaceAll]);
  buf := StringReplace(buf, 'nop'#13#10, 'n', [rfReplaceAll]);
  buf := StringReplace(buf, 'ret'#13#10, 'r', [rfReplaceAll]);

  CheckEquals
    ('rnrnrnrnnrnnrnnrnnnrnnnrnnnrnnnnrnnnnrnnnnrnnnnnrnnnnnrnnnnnr', buf);
end;

// calls
//
procedure TJITx86_64Tests.calls;
var
  offset: Integer;
  reg: TgpRegister64;
  expect: String;
begin
  for reg := gprRAX to gprR15 do
  begin
    for offset := 0 to 2 do
      FStream._call_reg(reg, offset * $40);
    expect := 'call qword ptr [' + cgpRegister64Name[reg];
    if reg in [gprRBP, gprRSP, gprR12, gprR13] then
      expect := expect + '+00h';
    expect := expect + ']'#13#10;
    CheckEquals(expect + 'call qword ptr [' + cgpRegister64Name[reg] +
      '+40h]'#13#10 + 'call qword ptr [' + cgpRegister64Name[reg] +
      '+00000080h]'#13#10, DisasmStream);
  end;
end;

// cmp_execmem_imm
//
procedure TJITx86_64Tests.cmp_execmem_imm;
begin
  FStream._cmp_execmem_imm(0, 0);
  FStream._cmp_execmem_imm(0, 1);
  FStream._cmp_execmem_imm(1, $10);
  FStream._cmp_execmem_imm(2, $112233);
  // FStream._cmp_execmem_imm(2, $123456789AB);    64bit compare is todo

  CheckEquals('cmp qword ptr [' + cgpRegister64Name[cExecMemGPR] +
    '+08h], 0000000000000000h'#13#10 + 'cmp qword ptr [' + cgpRegister64Name
    [cExecMemGPR] + '+08h], 01h'#13#10 + 'cmp qword ptr [' + cgpRegister64Name
    [cExecMemGPR] + '+20h], 10h'#13#10 + 'cmp qword ptr [' + cgpRegister64Name
    [cExecMemGPR] + '+38h], 0000000000112233h'#13#10, DisasmStream);
end;

{
  // cmp_dword_ptr_reg_reg
  //
  procedure TJITx86_64Tests.cmp_dword_ptr_reg_reg;
  var
  offset : Integer;
  dest, src : TgpRegister;
  expect : String;
  begin
  for dest:=gprEAX to gprEDI do begin
  for src:=gprEAX to gprEDI do begin
  expect:='';
  for offset:=0 to 2 do begin
  FStream._cmp_dword_ptr_reg_reg(dest, offset*$40, src);
  expect:=expect+'cmp dword ptr ['
  +cgpRegisterName[dest];
  case offset of
  1 : expect:=expect+'+40h';
  2 : expect:=expect+'+00000080h';
  else
  if dest=gprEBP then
  expect:=expect+'+00h';
  end;
  expect:=expect+'], '+cgpRegisterName[src]+#13#10;
  end;
  CheckEquals(expect, DisasmStream);
  end;
  end;
  end;
}
// cmp_reg_imm
//
procedure TJITx86_64Tests.cmp_reg_imm;
var
  reg: TgpRegister64;
  expect: String;
begin
  for reg := gprRAX to gprR15 do
  begin
    FStream._cmp_reg_imm(reg, $40);
    FStream._cmp_reg_imm(reg, $80);
    expect := 'cmp ' + cgpRegister64Name[reg] + ', 40h'#13#10 + 'cmp ' +
      cgpRegister64Name[reg] + ', 0000000000000080h'#13#10;
    CheckEquals(expect, DisasmStream);
  end;
end;

// ops
//
procedure TJITx86_64Tests.ops;
var
  reg, src: TgpRegister64;
  offset: Integer;
  expect, offsetText: String;
begin
  for offset := 0 to 2 do
  begin
    for src := gprRAX to gprR15 do
    begin
      case offset of
        1:
          offsetText := '+40h';
        2:
          offsetText := '+00000080h';
      else
        if src in [gprRBP, gprRSP, gprR12, gprR13] then
          offsetText := '+00h'
        else
          offsetText := '';
      end;
      for reg := gprRAX to gprR15 do
      begin
        expect := '';
        FStream._op_reg_qword_ptr_reg(gpOp_add, reg, src, offset * $40);
        expect := expect + 'add ' + cgpRegister64Name[reg] + ', qword ptr [' +
          cgpRegister64Name[src] + offsetText + ']'#13#10;
        FStream._op_reg_qword_ptr_reg(gpOp_adc, reg, src, offset * $40);
        expect := expect + 'adc ' + cgpRegister64Name[reg] + ', qword ptr [' +
          cgpRegister64Name[src] + offsetText + ']'#13#10;
        FStream._op_reg_qword_ptr_reg(gpOp_sub, reg, src, offset * $40);
        expect := expect + 'sub ' + cgpRegister64Name[reg] + ', qword ptr [' +
          cgpRegister64Name[src] + offsetText + ']'#13#10;
        FStream._op_reg_qword_ptr_reg(gpOp_sbb, reg, src, offset * $40);
        expect := expect + 'sbb ' + cgpRegister64Name[reg] + ', qword ptr [' +
          cgpRegister64Name[src] + offsetText + ']'#13#10;
        FStream._op_reg_qword_ptr_reg(gpOp_xor, reg, src, offset * $40);
        expect := expect + 'xor ' + cgpRegister64Name[reg] + ', qword ptr [' +
          cgpRegister64Name[src] + offsetText + ']'#13#10;
        FStream._op_reg_qword_ptr_reg(gpOp_and, reg, src, offset * $40);
        expect := expect + 'and ' + cgpRegister64Name[reg] + ', qword ptr [' +
          cgpRegister64Name[src] + offsetText + ']'#13#10;
        FStream._op_reg_qword_ptr_reg(gpOp_or, reg, src, offset * $40);
        expect := expect + 'or ' + cgpRegister64Name[reg] + ', qword ptr [' +
          cgpRegister64Name[src] + offsetText + ']'#13#10;
        FStream._op_reg_qword_ptr_reg(gpOp_cmp, reg, src, offset * $40);
        expect := expect + 'cmp ' + cgpRegister64Name[reg] + ', qword ptr [' +
          cgpRegister64Name[src] + offsetText + ']'#13#10;
        CheckEquals(expect, DisasmStream);
      end;
    end;
  end;
end;

// test_reg_reg
//
procedure TJITx86_64Tests.test_reg_reg;
var
  dest, src: TgpRegister64;
  expect: String;
begin
  for dest := gprRAX to gprR15 do
  begin
    expect := '';
    for src := gprRAX to gprR15 do
    begin
      FStream._test_reg_reg(dest, src);
      expect := expect + 'test ' + cgpRegister64Name[dest] + ', ' +
        cgpRegister64Name[src] + #13#10;
    end;
    CheckEquals(expect, DisasmStream);
  end;
end;

// test_reg_int32
//
procedure TJITx86_64Tests.test_reg_int32;
var
  reg: TgpRegister64;
  expect: String;
begin
  for reg := gprRAX to gprR15 do
  begin
    if reg in [gprRSP, gprRBP] then
      continue;
    FStream._test_reg_imm(reg, $40);
    FStream._test_reg_imm(reg, $180);
    case reg of
      gprRAX:
        expect := 'test ' + cgpRegister64bName[reg] + ', 40h'#13#10;
      gprRCX .. gprRDI:
        expect := 'test ' + cgpRegister64bName[reg] + ', 40h'#13#10;
      // gprRSP..gprRDI : expect:= 'test '+cgpRegister64dName[reg]+', 40h'#13#10;
    else
      expect := 'test ' + cgpRegister64bName[reg] + ', 40h'#13#10;
    end;
    expect := expect + 'test ' + cgpRegister64dName[reg] + ', 00000180h'#13#10;
    CheckEquals(expect, DisasmStream);
  end;
end;

{
  // test_dword_ptr_reg_int32
  //
  procedure TJITx86_64Tests.test_dword_ptr_reg_int32;
  var
  reg : TgpRegister;
  expect : String;
  begin
  for reg:=gprEAX to gprEDI do begin
  FStream._test_dword_ptr_reg_dword(reg, 0, $101);
  FStream._test_dword_ptr_reg_dword(reg, $40, $102);
  FStream._test_dword_ptr_reg_dword(reg, $80, $103);
  if reg=gprEBP then
  expect:='test dword ptr ['+cgpRegisterName[reg]+'+00h], 00000101h'#13#10
  else expect:='test dword ptr ['+cgpRegisterName[reg]+'], 00000101h'#13#10;
  expect:= expect
  +'test dword ptr ['+cgpRegisterName[reg]+'+40h], 00000102h'#13#10
  +'test dword ptr ['+cgpRegisterName[reg]+'+00000080h], 00000103h'#13#10
  ;
  CheckEquals(expect, DisasmStream);
  end;
  end;

  // test_dword_ptr_reg_byte
  //
  procedure TJITx86_64Tests.test_dword_ptr_reg_byte;
  var
  reg : TgpRegister;
  expect : String;
  begin
  for reg:=gprEAX to gprEDI do begin
  FStream._test_dword_ptr_reg_byte(reg, 0, 0);
  FStream._test_dword_ptr_reg_byte(reg, $40, 1);
  FStream._test_dword_ptr_reg_byte(reg, $80, 2);
  if reg=gprEBP then
  expect:='test byte ptr ['+cgpRegisterName[reg]+'+00h], 00000000h'#13#10
  else expect:='test byte ptr ['+cgpRegisterName[reg]+'], 00000000h'#13#10;
  expect:= expect
  +'test byte ptr ['+cgpRegisterName[reg]+'+40h], 00000001h'#13#10
  +'test byte ptr ['+cgpRegisterName[reg]+'+00000080h], 00000002h'#13#10
  ;
  CheckEquals(expect, DisasmStream);
  end;
  end;

  // test_dword_ptr_reg_reg
  //
  procedure TJITx86_64Tests.test_dword_ptr_reg_reg;
  var
  offset : Integer;
  dest, src : TgpRegister;
  expect : String;
  begin
  for dest:=gprEAX to gprEDI do begin
  for src:=gprEAX to gprEDI do begin
  expect:='';
  for offset:=0 to 2 do begin
  FStream._test_dword_ptr_reg_reg(dest, offset*$40, src);
  expect:=expect+'test dword ptr ['
  +cgpRegisterName[dest];
  case offset of
  1 : expect:=expect+'+40h';
  2 : expect:=expect+'+00000080h';
  else
  if dest=gprEBP then
  expect:=expect+'+00h';
  end;
  expect:=expect+'], '+cgpRegisterName[src]+#13#10;
  end;
  CheckEquals(expect, DisasmStream);
  end;
  end;
  end;

  // and_or_byte
  //
  procedure TJITx86_64Tests.and_or_byte;
  var
  reg : TgpRegister;
  expect : String;
  begin
  for reg:=gprEAX to gprEDI do begin
  FStream._and_dword_ptr_reg_byte(reg, 0, 0);
  FStream._or_dword_ptr_reg_byte(reg, 0, 0);
  FStream._and_dword_ptr_reg_byte(reg, $40, 1);
  FStream._or_dword_ptr_reg_byte(reg, $40, 1);
  FStream._and_dword_ptr_reg_byte(reg, $80, 2);
  FStream._or_dword_ptr_reg_byte(reg, $80, 2);
  if reg=gprEBP then
  expect:= 'and byte ptr ['+cgpRegisterName[reg]+'+00h], 00000000h'#13#10
  +'or byte ptr ['+cgpRegisterName[reg]+'+00h], 00000000h'#13#10
  else expect:='and byte ptr ['+cgpRegisterName[reg]+'], 00000000h'#13#10
  +'or byte ptr ['+cgpRegisterName[reg]+'], 00000000h'#13#10;
  expect:= expect
  +'and byte ptr ['+cgpRegisterName[reg]+'+40h], 00000001h'#13#10
  +'or byte ptr ['+cgpRegisterName[reg]+'+40h], 00000001h'#13#10
  +'and byte ptr ['+cgpRegisterName[reg]+'+00000080h], 00000002h'#13#10
  +'or byte ptr ['+cgpRegisterName[reg]+'+00000080h], 00000002h'#13#10
  ;
  CheckEquals(expect, DisasmStream);
  end;
  end;
}
// boolflags
//
procedure TJITx86_64Tests.boolflags;
begin
  FStream._set_al_flags(NegateBoolFlags(flagsA));
  FStream._set_al_flags(NegateBoolFlags(flagsAE));
  FStream._set_al_flags(NegateBoolFlags(flagsB));
  FStream._set_al_flags(NegateBoolFlags(flagsBE));
  FStream._set_al_flags(NegateBoolFlags(flagsE));
  FStream._set_al_flags(NegateBoolFlags(flagsG));
  FStream._set_al_flags(NegateBoolFlags(flagsGE));
  FStream._set_al_flags(NegateBoolFlags(flagsL));
  FStream._set_al_flags(NegateBoolFlags(flagsLE));
  FStream._set_al_flags(NegateBoolFlags(flagsNE));
  FStream._set_al_flags(NegateBoolFlags(flagsNO));
  FStream._set_al_flags(NegateBoolFlags(flagsNP));
  FStream._set_al_flags(NegateBoolFlags(flagsNS));
  FStream._set_al_flags(NegateBoolFlags(flagsO));
  FStream._set_al_flags(NegateBoolFlags(flagsP));
  FStream._set_al_flags(NegateBoolFlags(flagsS));

  CheckEquals('setbe al'#13#10 + 'setb al'#13#10 + 'setnb al'#13#10 +
    'setnbe al'#13#10 + 'setne al'#13#10 + 'setle al'#13#10 + 'setl al'#13#10 +
    'setnl al'#13#10 + 'setnle al'#13#10 + 'sete al'#13#10 + 'seto al'#13#10 +
    'setp al'#13#10 + 'sets al'#13#10 + 'setno al'#13#10 + 'setnp al'#13#10 +
    'setns al'#13#10, DisasmStream);
end;

// movsd_indexed
//
procedure TJITx86_64Tests.movsd_indexed;
var
  offset, scale: Integer;
  base, index: TgpRegister64;
  dest: TxmmRegister;
  expect: String;
begin
  for dest := xmm0 to xmm15 do
  begin
    for base := gprRAX to gprR15 do
    begin
      for index := gprRAX to gprR15 do
      begin
        if index in [gprRSP, gprR12] then
          continue;
        expect := '';
        for offset := 0 to 2 do
        begin
          for scale := 0 to 3 do
          begin
            FStream._movsd_reg_qword_ptr_indexed(dest, base, index, 1 shl scale,
              offset * $40);
            expect := expect + 'movsd xmm' + IntToStr(Ord(dest)) +
              ', qword ptr [' + cgpRegister64Name[base] + '+' +
              cgpRegister64Name[index];
            case scale of
              1:
                expect := expect + '*2';
              2:
                expect := expect + '*4';
              3:
                expect := expect + '*8';
            end;
            case offset of
              1:
                expect := expect + '+40h';
              2:
                expect := expect + '+00000080h';
            else
              if base in [gprRBP, gprRSP, gprR12, gprR13] then
                expect := expect + '+00h';
            end;
            expect := expect + ']'#13#10;
          end;
        end;
        CheckEquals(expect, DisasmStream);
        expect := '';
        for offset := 0 to 2 do
        begin
          for scale := 0 to 3 do
          begin
            FStream._movsd_qword_ptr_indexed_reg(base, index, 1 shl scale,
              offset * $40, dest);
            expect := expect + 'movsd qword ptr [' + cgpRegister64Name[base] +
              '+' + cgpRegister64Name[index];
            case scale of
              1:
                expect := expect + '*2';
              2:
                expect := expect + '*4';
              3:
                expect := expect + '*8';
            end;
            case offset of
              1:
                expect := expect + '+40h';
              2:
                expect := expect + '+00000080h';
            else
              if base in [gprRBP, gprRSP, gprR12, gprR13] then
                expect := expect + '+00h';
            end;
            expect := expect + '], xmm' + IntToStr(Ord(dest)) + #13#10;
          end;
        end;
        CheckEquals(expect, DisasmStream);
      end;
    end;
  end;
end;

// mov_indexed
//
procedure TJITx86_64Tests.mov_indexed;
var
  offset, scale: Integer;
  dest, base, index: TgpRegister64;
  expect: String;
begin
  for dest := gprRAX to gprR15 do
  begin
    for base := gprRAX to gprR15 do
    begin
      for index := gprRAX to gprR15 do
      begin
        if index in [gprRSP, gprR12] then
          continue;
        expect := '';
        for offset := 0 to 2 do
        begin
          for scale := 0 to 3 do
          begin
            FStream._mov_reg_qword_ptr_indexed(dest, base, index, 1 shl scale,
              offset * $40);
            expect := expect + 'mov ' + cgpRegister64Name[dest] +
              ', qword ptr [' + cgpRegister64Name[base] + '+' +
              cgpRegister64Name[index];
            case scale of
              1:
                expect := expect + '*2';
              2:
                expect := expect + '*4';
              3:
                expect := expect + '*8';
            end;
            case offset of
              1:
                expect := expect + '+40h';
              2:
                expect := expect + '+00000080h';
            else
              if base in [gprRBP, gprRSP, gprR12, gprR13] then
                expect := expect + '+00h';
            end;
            expect := expect + ']'#13#10;
          end;
        end;
        CheckEquals(expect, DisasmStream);
      end;
    end;
  end;
end;

// movsx
//
procedure TJITx86_64Tests.movsx;
begin
  FStream._movsx_reg_al(gprRAX);
  FStream._movsx_reg_al(gprRDX);
  FStream._movsx_reg_al(gprR9);
  FStream._movsx_reg_al(gprR15);
  CheckEquals('' + 'movsx rax, al'#13#10 + 'movsx rdx, al'#13#10 +
    'movsx r9, al'#13#10 + 'movsx r15, al'#13#10, DisasmStream);
end;

// lea
//
procedure TJITx86_64Tests.lea;
begin
  FStream._lea_reg_ptr_indexed_reg(gprRAX, gprRCX, gprRDX, 2, 3);
  FStream._lea_reg_ptr_indexed_reg(gprR9, gprRBX, gprRBP, 1, 456);
  FStream._lea_reg_ptr_indexed_reg(gprRSP, gprR10, gprRAX, 4, 0);
  FStream._lea_reg_ptr_indexed_reg(gprRDX, gprRDI, gprR15, 8, -5);
  CheckEquals('' + 'lea rax, qword ptr [rcx+rdx*2+03h]'#13#10 +
    'lea r9, qword ptr [rbx+rbp+000001C8h]'#13#10 +
    'lea rsp, qword ptr [r10+rax*4]'#13#10 +
    'lea rdx, qword ptr [rdi+r15*8-05h]'#13#10, DisasmStream);

  FStream._lea_reg_reg(gprRAX, gprRCX, 3);
  FStream._lea_reg_reg(gprR9, gprRBP, 456);
  FStream._lea_reg_reg(gprRSP, gprR10, 0);
  FStream._lea_reg_reg(gprRDX, gprRDI, -5);
  FStream._lea_reg_reg(gprRBP, gprRBP, 128);
  CheckEquals('' + 'lea rax, qword ptr [rcx+03h]'#13#10 +
    'lea r9, qword ptr [rbp+000001C8h]'#13#10 + 'lea rsp, qword ptr [r10]'#13#10
    + 'lea rdx, qword ptr [rdi-05h]'#13#10 +
    'lea rbp, qword ptr [rbp+00000080h]'#13#10, DisasmStream);
end;

// vcmp
//
procedure TJITx86_64Tests.vcmp;
begin
  FStream._vcmpps(ymm0, ymm0, ymm0, 0);
  FStream._vcmpps(ymm1, ymm2, ymm3, 1);
  FStream._vcmpps(ymm5, ymm0, ymm7, 2);
  FStream._vcmpps(ymm9, ymm10, ymm15, 3);
  FStream._vcmpps(ymm10, ymm7, ymm5, 4);
  FStream._vcmpps(ymm3, ymm4, ymm8, 5);
  CheckEquals('' + 'vcmpeqps ymm0, ymm0, ymm0, 00h'#13#10 +
    'vcmpltps ymm1, ymm2, ymm3, 01h'#13#10 +
    'vcmpleps ymm5, ymm0, ymm7, 02h'#13#10 +
    'vcmpunordps ymm9, ymm10, ymm15, 03h'#13#10 +
    'vcmpneqps ymm10, ymm7, ymm5, 04h'#13#10 +
    'vcmpnltps ymm3, ymm4, ymm8, 05h'#13#10, DisasmStream);

  FStream._vcmppd(ymm0, ymm0, ymm0, 0);
  FStream._vcmppd(ymm1, ymm2, ymm3, 1);
  FStream._vcmppd(ymm5, ymm0, ymm7, 2);
  FStream._vcmppd(ymm9, ymm10, ymm15, 3);
  FStream._vcmppd(ymm10, ymm7, ymm5, 4);
  FStream._vcmppd(ymm3, ymm4, ymm8, 5);
  CheckEquals('' + 'vcmpeqpd ymm0, ymm0, ymm0, 00h'#13#10 +
    'vcmpltpd ymm1, ymm2, ymm3, 01h'#13#10 +
    'vcmplepd ymm5, ymm0, ymm7, 02h'#13#10 +
    'vcmpunordpd ymm9, ymm10, ymm15, 03h'#13#10 +
    'vcmpneqpd ymm10, ymm7, ymm5, 04h'#13#10 +
    'vcmpnltpd ymm3, ymm4, ymm8, 05h'#13#10, DisasmStream);
end;

// vpcmpeq
//
procedure TJITx86_64Tests.vpcmpeq;
begin
  FStream._vpcmpeqd(ymm0, ymm0, ymm0);
  FStream._vpcmpeqd(ymm1, ymm2, ymm3);
  FStream._vpcmpeqd(ymm5, ymm0, ymm7);
  FStream._vpcmpeqd(ymm9, ymm10, ymm15);
  FStream._vpcmpeqd(ymm10, ymm7, ymm5);
  FStream._vpcmpeqd(ymm3, ymm4, ymm8);
  CheckEquals('' + 'vpcmpeqd ymm0, ymm0, ymm0'#13#10 +
    'vpcmpeqd ymm1, ymm2, ymm3'#13#10 + 'vpcmpeqd ymm5, ymm0, ymm7'#13#10 +
    'vpcmpeqd ymm9, ymm10, ymm15'#13#10 + 'vpcmpeqd ymm10, ymm7, ymm5'#13#10 +
    'vpcmpeqd ymm3, ymm4, ymm8'#13#10, DisasmStream);

  FStream._vpcmpeqq(ymm0, ymm0, ymm0);
  FStream._vpcmpeqq(ymm1, ymm2, ymm3);
  FStream._vpcmpeqq(ymm5, ymm0, ymm7);
  FStream._vpcmpeqq(ymm9, ymm10, ymm15);
  FStream._vpcmpeqq(ymm10, ymm7, ymm5);
  FStream._vpcmpeqq(ymm3, ymm4, ymm8);
  CheckEquals('' + 'vpcmpeqq ymm0, ymm0, ymm0'#13#10 +
    'vpcmpeqq ymm1, ymm2, ymm3'#13#10 + 'vpcmpeqq ymm5, ymm0, ymm7'#13#10 +
    'vpcmpeqq ymm9, ymm10, ymm15'#13#10 + 'vpcmpeqq ymm10, ymm7, ymm5'#13#10 +
    'vpcmpeqq ymm3, ymm4, ymm8'#13#10, DisasmStream);
end;

// vpblendvb
//
procedure TJITx86_64Tests.vblend;
begin
  FStream._vpblendvb(ymm0, ymm0, ymm0, ymm0);
  FStream._vpblendvb(ymm1, ymm2, ymm3, ymm4);
  FStream._vpblendvb(ymm5, ymm0, ymm7, ymm2);
  FStream._vpblendvb(ymm9, ymm10, ymm15, ymm12);
  FStream._vpblendvb(ymm1, ymm2, ymm8, ymm9);

  FStream._vblendvps(ymm0, ymm0, ymm0, ymm0);
  FStream._vblendvps(ymm1, ymm2, ymm3, ymm4);
  FStream._vblendvps(ymm5, ymm0, ymm7, ymm2);
  FStream._vblendvps(ymm9, ymm10, ymm15, ymm12);
  FStream._vblendvps(ymm1, ymm2, ymm8, ymm9);

  FStream._vblendvpd(ymm0, ymm0, ymm0, ymm0);
  FStream._vblendvpd(ymm1, ymm2, ymm3, ymm4);
  FStream._vblendvpd(ymm5, ymm0, ymm7, ymm2);
  FStream._vblendvpd(ymm9, ymm10, ymm15, ymm12);
  FStream._vblendvpd(ymm1, ymm2, ymm8, ymm9);
  CheckEquals('' + 'vpblendvb ymm0, ymm0, ymm0, ymm0'#13#10 +
    'vpblendvb ymm1, ymm2, ymm3, ymm4'#13#10 +
    'vpblendvb ymm5, ymm0, ymm7, ymm2'#13#10 +
    'vpblendvb ymm9, ymm10, ymm15, ymm12'#13#10 +
    'vpblendvb ymm1, ymm2, ymm8, ymm9'#13#10 +
    'vblendvps ymm0, ymm0, ymm0, ymm0'#13#10 +
    'vblendvps ymm1, ymm2, ymm3, ymm4'#13#10 +
    'vblendvps ymm5, ymm0, ymm7, ymm2'#13#10 +
    'vblendvps ymm9, ymm10, ymm15, ymm12'#13#10 +
    'vblendvps ymm1, ymm2, ymm8, ymm9'#13#10 +
    'vblendvpd ymm0, ymm0, ymm0, ymm0'#13#10 +
    'vblendvpd ymm1, ymm2, ymm3, ymm4'#13#10 +
    'vblendvpd ymm5, ymm0, ymm7, ymm2'#13#10 +
    'vblendvpd ymm9, ymm10, ymm15, ymm12'#13#10 +
    'vblendvpd ymm1, ymm2, ymm8, ymm9'#13#10, DisasmStream);
end;

// vbroadcast
//
procedure TJITx86_64Tests.vbroadcast;
begin
  FStream._vbroadcastsd(ymm0, xmm0);
  FStream._vbroadcastsd(ymm1, xmm2);
  FStream._vbroadcastsd(ymm9, xmm12);
  FStream._vbroadcastsd_ptr_reg(ymm0, gprRAX, 0);
  FStream._vbroadcastsd_ptr_reg(ymm9, gprRCX, 123);
  FStream._vbroadcastsd_ptr_reg(ymm2, gprR8, 123456);
  FStream._vbroadcastsd_ptr_reg(ymm8, gprR9, -5);
  CheckEquals('' + 'vbroadcastsd ymm0, xmm0'#13#10 +
    'vbroadcastsd ymm1, xmm2'#13#10 + 'vbroadcastsd ymm9, xmm12'#13#10 +
    'vbroadcastsd ymm0, qword ptr [rax]'#13#10 +
    'vbroadcastsd ymm9, qword ptr [rcx+7Bh]'#13#10 +
    'vbroadcastsd ymm2, qword ptr [r8+0001E240h]'#13#10 +
    'vbroadcastsd ymm8, qword ptr [r9-05h]'#13#10, DisasmStream);

  FStream._vbroadcastss(ymm0, xmm0);
  FStream._vbroadcastss(ymm1, xmm2);
  FStream._vbroadcastss(ymm9, xmm12);
  FStream._vbroadcastss_ptr_reg(ymm0, gprRAX, 0);
  FStream._vbroadcastss_ptr_reg(ymm9, gprRCX, 123);
  FStream._vbroadcastss_ptr_reg(ymm2, gprR8, 123456);
  FStream._vbroadcastss_ptr_reg(ymm8, gprR9, -5);
  CheckEquals('' + 'vbroadcastss ymm0, xmm0'#13#10 +
    'vbroadcastss ymm1, xmm2'#13#10 + 'vbroadcastss ymm9, xmm12'#13#10 +
    'vbroadcastss ymm0, dword ptr [rax]'#13#10 +
    'vbroadcastss ymm9, dword ptr [rcx+7Bh]'#13#10 +
    'vbroadcastss ymm2, dword ptr [r8+0001E240h]'#13#10 +
    'vbroadcastss ymm8, dword ptr [r9-05h]'#13#10, DisasmStream);

  FStream._vpbroadcastd_ptr_reg(ymm0, gprRAX, 0);
  FStream._vpbroadcastd_ptr_reg(ymm9, gprRCX, 123);
  FStream._vpbroadcastd_ptr_reg(ymm2, gprR8, 123456);
  FStream._vpbroadcastd_ptr_reg(ymm8, gprR9, -5);
  CheckEquals('' + 'vpbroadcastd ymm0, dword ptr [rax]'#13#10 +
    'vpbroadcastd ymm9, dword ptr [rcx+7Bh]'#13#10 +
    'vpbroadcastd ymm2, dword ptr [r8+0001E240h]'#13#10 +
    'vpbroadcastd ymm8, dword ptr [r9-05h]'#13#10, DisasmStream);

  FStream._vpbroadcastq_ptr_reg(ymm0, gprRAX, 0);
  FStream._vpbroadcastq_ptr_reg(ymm9, gprRCX, 123);
  FStream._vpbroadcastq_ptr_reg(ymm2, gprR8, 123456);
  FStream._vpbroadcastq_ptr_reg(ymm8, gprR9, -5);
  FStream._vpbroadcastq_ptr_indexed(ymm0, gprRAX, gprRAX, 1, 0);
  FStream._vpbroadcastq_ptr_indexed(ymm8, gprRAX, gprRCX, 1, 1);
  FStream._vpbroadcastq_ptr_indexed(ymm9, gprRCX, gprRDI, 2, 12);
  FStream._vpbroadcastq_ptr_indexed(ymm7, gprRCX, gprRDI, 2, 123456);
  CheckEquals('' + 'vpbroadcastq ymm0, qword ptr [rax]'#13#10 +
    'vpbroadcastq ymm9, qword ptr [rcx+7Bh]'#13#10 +
    'vpbroadcastq ymm2, qword ptr [r8+0001E240h]'#13#10 +
    'vpbroadcastq ymm8, qword ptr [r9-05h]'#13#10 +
    'vpbroadcastq ymm0, qword ptr [rax+rax]'#13#10 +
    'vpbroadcastq ymm8, qword ptr [rax+rcx+01h]'#13#10 +
    'vpbroadcastq ymm9, qword ptr [rcx+rdi*2+0Ch]'#13#10 +
    'vpbroadcastq ymm7, qword ptr [rcx+rdi*2+0001E240h]'#13#10, DisasmStream);
end;

// v_op_pd
//
procedure TJITx86_64Tests.v_op_pd;
begin
  FStream._v_op_pd(xmm_xorpd, ymm0, ymm1, ymm2);
  FStream._v_op_pd(xmm_addpd, ymm8, ymm9, ymm10);
  FStream._v_op_pd(xmm_mulpd, ymm15, ymm0, ymm9);
  FStream._v_op_pd(xmm_minpd, ymm1, ymm12, ymm5);
  FStream._v_op_pd(xmm_maxpd, ymm8, ymm0, ymm12);
  FStream._v_op_pd(xmm_subpd, ymm2, ymm11, ymm13);
  CheckEquals('' + 'vxorpd ymm0, ymm1, ymm2'#13#10 +
    'vaddpd ymm8, ymm9, ymm10'#13#10 + 'vmulpd ymm15, ymm0, ymm9'#13#10 +
    'vminpd ymm1, ymm12, ymm5'#13#10 + 'vmaxpd ymm8, ymm0, ymm12'#13#10 +
    'vsubpd ymm2, ymm11, ymm13'#13#10, DisasmStream);
end;

// v_op_ps
//
procedure TJITx86_64Tests.v_op_ps;
begin
  FStream._v_op_ps(xmm_xorps, ymm0, ymm1, ymm2);
  FStream._v_op_ps(xmm_addps, ymm8, ymm9, ymm10);
  FStream._v_op_ps(xmm_mulps, ymm15, ymm0, ymm9);
  FStream._v_op_ps(xmm_minps, ymm1, ymm12, ymm5);
  FStream._v_op_ps(xmm_maxps, ymm8, ymm0, ymm12);
  FStream._v_op_ps(xmm_subps, ymm2, ymm11, ymm13);
  CheckEquals('' + 'vxorps ymm0, ymm1, ymm2'#13#10 +
    'vaddps ymm8, ymm9, ymm10'#13#10 + 'vmulps ymm15, ymm0, ymm9'#13#10 +
    'vminps ymm1, ymm12, ymm5'#13#10 + 'vmaxps ymm8, ymm0, ymm12'#13#10 +
    'vsubps ymm2, ymm11, ymm13'#13#10, DisasmStream);
end;

// _movups_ptr_reg
//
procedure TJITx86_64Tests._movups_ptr_reg;
begin
  FStream._movups_reg_ptr_reg(xmm0, gprRAX, 0);
  FStream._movups_reg_ptr_reg(xmm8, gprRBP, 1);
  FStream._movups_reg_ptr_reg(xmm1, gprRDX, 123456);
  FStream._movups_ptr_reg_reg(gprRAX, 0, xmm0);
  FStream._movups_ptr_reg_reg(gprRBP, 1, xmm8);
  FStream._movups_ptr_reg_reg(gprRDX, 123456, xmm1);
  FStream._movups_ptr_reg_reg(gprRBP, -$D0, xmm9);
  CheckEquals('' + 'movups xmm0, xmmword ptr [rax]'#13#10 +
    'movups xmm8, xmmword ptr [rbp+01h]'#13#10 +
    'movups xmm1, xmmword ptr [rdx+0001E240h]'#13#10 +
    'movups xmmword ptr [rax], xmm0'#13#10 +
    'movups xmmword ptr [rbp+01h], xmm8'#13#10 +
    'movups xmmword ptr [rdx+0001E240h], xmm1'#13#10 +
    'movups xmmword ptr [rbp-000000D0h], xmm9'#13#10, DisasmStream);

end;

// _movups_ptr_reg_reg
//
procedure TJITx86_64Tests._movups_ptr_reg_reg;
begin
  // xmm8+ actually works, but Bea does not disassemble them correctly

  FStream._movups_reg_ptr_reg_reg(xmm0, gprRAX, gprRAX);
  // FStream._movups_reg_ptr_reg_reg(xmm8, gprR9, gprRCX);
  FStream._movups_reg_ptr_reg_reg(xmm1, gprRDX, gprR8);
  // FStream._movups_reg_ptr_reg_reg(xmm9, gprR10, gprR11);
  CheckEquals('' + 'movups xmm0, xmmword ptr [rax+rax]'#13#10
    // + 'movups xmm8, xmmword ptr [r9+rcx]'#13#10
    + 'movups xmm1, xmmword ptr [rdx+r8]'#13#10
    // + 'movups xmm9, xmmword ptr [r10+r11]'#13#10
    , DisasmStream);
end;

// _vmovdqu
//
procedure TJITx86_64Tests._vmovdqu;
begin
  FStream._vmovdqu_ptr_reg(ymm0, gprRAX, 0);
  FStream._vmovdqu_ptr_reg(ymm8, gprRBP, 1);
  FStream._vmovdqu_ptr_reg(ymm1, gprRDX, 123456);
  FStream._vmovdqu_ptr_indexed(ymm0, gprRAX, gprRAX, 1, 0);
  FStream._vmovdqu_ptr_indexed(ymm8, gprRAX, gprRCX, 1, 1);
  FStream._vmovdqu_ptr_indexed(ymm9, gprRCX, gprRDI, 2, 12);
  FStream._vmovdqu_ptr_indexed(ymm7, gprRCX, gprRDI, 2, 123456);
  CheckEquals('' + 'vmovdqu ymm0, ymmword ptr [rax]'#13#10 +
    'vmovdqu ymm8, ymmword ptr [rbp+01h]'#13#10 +
    'vmovdqu ymm1, ymmword ptr [rdx+0001E240h]'#13#10 +
    'vmovdqu ymm0, ymmword ptr [rax+rax]'#13#10 +
    'vmovdqu ymm8, ymmword ptr [rax+rcx+01h]'#13#10 +
    'vmovdqu ymm9, ymmword ptr [rcx+rdi*2+0Ch]'#13#10 +
    'vmovdqu ymm7, ymmword ptr [rcx+rdi*2+0001E240h]'#13#10, DisasmStream);
end;

// _vmovapd
//
procedure TJITx86_64Tests._vmovapd;
begin
  FStream._vmovapd_reg_reg(ymm0, ymm1);
  FStream._vmovapd_reg_reg(ymm10, ymm3);
  FStream._vmovapd_reg_reg(ymm4, ymm14);
  FStream._vmovapd_reg_reg(ymm9, ymm8);
  FStream._vmovapd_reg_reg(ymm8, ymm7);
  CheckEquals('' + 'vmovapd ymm0, ymm1'#13#10 + 'vmovapd ymm10, ymm3'#13#10 +
    'vmovapd ymm4, ymm14'#13#10 + 'vmovapd ymm9, ymm8'#13#10 +
    'vmovapd ymm8, ymm7'#13#10, DisasmStream);
end;

// _vmovupd_ptr_indexed
//
procedure TJITx86_64Tests._vmovupd_ptr_indexed;
begin
  FStream._vmovupd_ptr_indexed(ymm0, gprRAX, gprRAX, 1, 0);
  FStream._vmovupd_ptr_indexed(ymm8, gprRAX, gprRCX, 1, 0);
  FStream._vmovupd_ptr_indexed(ymm8, gprRAX, gprRCX, 1, 1);
  FStream._vmovupd_ptr_indexed(ymm9, gprRCX, gprRDI, 2, 12);
  FStream._vmovupd_ptr_indexed(ymm7, gprRCX, gprRDI, 2, 123456);
  CheckEquals('' + 'vmovupd ymm0, ymmword ptr [rax+rax]'#13#10 +
    'vmovupd ymm8, ymmword ptr [rax+rcx]'#13#10 +
    'vmovupd ymm8, ymmword ptr [rax+rcx+01h]'#13#10 +
    'vmovupd ymm9, ymmword ptr [rcx+rdi*2+0Ch]'#13#10 +
    'vmovupd ymm7, ymmword ptr [rcx+rdi*2+0001E240h]'#13#10, DisasmStream);
end;

// _vmovups_ptr_indexed
//
procedure TJITx86_64Tests._vmovups_ptr_indexed;
begin
  FStream._vmovups_ptr_indexed(ymm0, gprRAX, gprRAX, 1, 0);
  FStream._vmovups_ptr_indexed(ymm8, gprRAX, gprRCX, 1, 0);
  FStream._vmovups_ptr_indexed(ymm8, gprRAX, gprRCX, 1, 1);
  FStream._vmovups_ptr_indexed(ymm9, gprRCX, gprRDI, 2, 12);
  FStream._vmovups_ptr_indexed(ymm7, gprRCX, gprRDI, 2, 123456);
  CheckEquals('' + 'vmovups ymm0, ymmword ptr [rax+rax]'#13#10 +
    'vmovups ymm8, ymmword ptr [rax+rcx]'#13#10 +
    'vmovups ymm8, ymmword ptr [rax+rcx+01h]'#13#10 +
    'vmovups ymm9, ymmword ptr [rcx+rdi*2+0Ch]'#13#10 +
    'vmovups ymm7, ymmword ptr [rcx+rdi*2+0001E240h]'#13#10, DisasmStream);
end;

// _vmovupd_ptr_reg
//
procedure TJITx86_64Tests._vmovupd_ptr_reg;
begin
  FStream._vmovupd_ptr_reg(ymm0, gprRAX, 0);
  FStream._vmovupd_ptr_reg(ymm8, gprRBP, 1);
  FStream._vmovupd_ptr_reg(ymm1, gprRDX, 123456);
  FStream._vmovupd_ptr_reg_reg(gprRAX, 0, ymm0);
  FStream._vmovupd_ptr_reg_reg(gprRBP, 1, ymm8);
  FStream._vmovupd_ptr_reg_reg(gprRDX, 123456, ymm1);
  FStream._vmovupd_ptr_reg_reg(gprRBP, -$D0, ymm9);
  CheckEquals('' + 'vmovupd ymm0, ymmword ptr [rax]'#13#10 +
    'vmovupd ymm8, ymmword ptr [rbp+01h]'#13#10 +
    'vmovupd ymm1, ymmword ptr [rdx+0001E240h]'#13#10 +
    'vmovupd ymmword ptr [rax], ymm0'#13#10 +
    'vmovupd ymmword ptr [rbp+01h], ymm8'#13#10 +
    'vmovupd ymmword ptr [rdx+0001E240h], ymm1'#13#10 +
    'vmovupd ymmword ptr [rbp-000000D0h], ymm9'#13#10, DisasmStream);
end;

// _vmovups_ptr_reg
//
procedure TJITx86_64Tests._vmovups_ptr_reg;
begin
  FStream._vmovups_ptr_reg(ymm0, gprRAX, 0);
  FStream._vmovups_ptr_reg(ymm8, gprRBP, 1);
  FStream._vmovups_ptr_reg(ymm1, gprRDX, 123456);
  FStream._vmovups_ptr_reg_reg(gprRAX, 0, ymm0);
  FStream._vmovups_ptr_reg_reg(gprRBP, 1, ymm8);
  FStream._vmovups_ptr_reg_reg(gprRDX, 123456, ymm1);
  FStream._vmovups_ptr_reg_reg(gprRBP, -$D0, ymm9);
  CheckEquals('' + 'vmovups ymm0, ymmword ptr [rax]'#13#10 +
    'vmovups ymm8, ymmword ptr [rbp+01h]'#13#10 +
    'vmovups ymm1, ymmword ptr [rdx+0001E240h]'#13#10 +
    'vmovups ymmword ptr [rax], ymm0'#13#10 +
    'vmovups ymmword ptr [rbp+01h], ymm8'#13#10 +
    'vmovups ymmword ptr [rdx+0001E240h], ymm1'#13#10 +
    'vmovups ymmword ptr [rbp-000000D0h], ymm9'#13#10, DisasmStream);
end;

// _vfma
//
procedure TJITx86_64Tests._vfma;
begin
  FStream._vfmadd_pd(231, ymm0, ymm1, ymm2);
  FStream._vfmadd_pd(213, ymm9, ymm0, ymm0);
  FStream._vfmadd_pd(132, ymm1, ymm9, ymm1);
  FStream._vfmadd_pd(213, ymm10, ymm8, ymm3);
  FStream._vfmadd_pd(231, ymm11, ymm10, ymm12);

  FStream._vfmadd_ps(231, ymm0, ymm1, ymm2);
  FStream._vfmadd_ps(213, ymm9, ymm0, ymm0);
  FStream._vfmadd_ps(132, ymm1, ymm9, ymm1);
  FStream._vfmadd_ps(213, ymm10, ymm8, ymm3);
  FStream._vfmadd_ps(231, ymm11, ymm10, ymm12);

  CheckEquals('' + 'vfmadd231pd ymm0, ymm1, ymm2'#13#10 +
    'vfmadd213pd ymm9, ymm0, ymm0'#13#10 + 'vfmadd132pd ymm1, ymm9, ymm1'#13#10
    + 'vfmadd213pd ymm10, ymm8, ymm3'#13#10 +
    'vfmadd231pd ymm11, ymm10, ymm12'#13#10 +
    'vfmadd231ps ymm0, ymm1, ymm2'#13#10 + 'vfmadd213ps ymm9, ymm0, ymm0'#13#10
    + 'vfmadd132ps ymm1, ymm9, ymm1'#13#10 +
    'vfmadd213ps ymm10, ymm8, ymm3'#13#10 +
    'vfmadd231ps ymm11, ymm10, ymm12'#13#10, DisasmStream);
end;

// _vpround
//
procedure TJITx86_64Tests._vpround;
begin
  FStream._vround_pd(ymm0, ymm1, 0);
  FStream._vround_pd(ymm9, ymm3, 1);
  FStream._vround_pd(ymm1, ymm10, 127);
  FStream._vround_pd(ymm12, ymm15, 255);

  FStream._vround_ps(ymm0, ymm1, 0);
  FStream._vround_ps(ymm9, ymm3, 1);
  FStream._vround_ps(ymm1, ymm10, 127);
  FStream._vround_ps(ymm12, ymm15, 255);

  CheckEquals('' + 'vroundpd ymm0, ymm1, 00h'#13#10 +
    'vroundpd ymm9, ymm3, 01h'#13#10 + 'vroundpd ymm1, ymm10, 7Fh'#13#10 +
    'vroundpd ymm12, ymm15, FFh'#13#10 + 'vroundps ymm0, ymm1, 00h'#13#10 +
    'vroundps ymm9, ymm3, 01h'#13#10 + 'vroundps ymm1, ymm10, 7Fh'#13#10 +
    'vroundps ymm12, ymm15, FFh'#13#10, DisasmStream);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

RegisterTest('jitx86_64Tests', TJITx86_64Tests);

{$ELSE}

implementation

{$IFEND}

end.
