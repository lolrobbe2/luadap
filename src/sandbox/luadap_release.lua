local a = require("socket")
local b = {}
b.__index = b; Luadap = {}
local c = nil; local d = nil; local e = {}
e.__index = e; local f = require "debug"
local os = os or (function(g)
  local h, i = pcall(require, g)
  return h and i or nil
end)("os")
local j = false; local k = false; local l = 'json'
local pairs, type, tostring, tonumber, getmetatable, setmetatable = pairs, type, tostring, tonumber, getmetatable,
    setmetatable; local error, require, pcall, select = error, require, pcall, select; local m, n = math.floor, math
.huge; local o, p, q, r, s, t, u, v = string.rep, string.gsub, string.sub, string.byte, string.char, string.find,
    string.len, string.format; local w = string.match; local x = table.concat; local y = { version = "dkjson 2.8" }
local z = {}
if k then if j then _G[l] = z else _G[l] = y end end; pcall(function()
  local A = require "debug".getmetatable; if A then getmetatable = A end
end)
y.null = setmetatable({}, { __tojson = function() return "null" end })
local function B(C)
  local D, E, F = 0, 0, 0; for G, H in pairs(C) do if G == 'n' and type(H) == 'number' then
      F = H; if H > D then D = H end
    else
      if type(G) ~= 'number' or G < 1 or m(G) ~= G then return false end; if G > D then D = G end; E = E + 1
    end end; if D > 10 and D > F and D > E * 2 then return false end; return true, D
end; local I = { ["\""] = "\\\"", ["\\"] = "\\\\", ["\b"] = "\\b", ["\f"] = "\\f", ["\n"] = "\\n", ["\r"] = "\\r", ["\t"] =
"\\t" }
local function J(K)
  local L = I[K]
  if L then return L end; local M, N, O, P = r(K, 1, 4)
  M, N, O, P = M or 0, N or 0, O or 0, P or 0; if M <= 0x7f then L = M elseif 0xc0 <= M and M <= 0xdf and N >= 0x80 then L = (M - 0xc0) *
    0x40 + N - 0x80 elseif 0xe0 <= M and M <= 0xef and N >= 0x80 and O >= 0x80 then L = ((M - 0xe0) * 0x40 + N - 0x80) *
    0x40 + O - 0x80 elseif 0xf0 <= M and M <= 0xf7 and N >= 0x80 and O >= 0x80 and P >= 0x80 then L = (((M - 0xf0) * 0x40 + N - 0x80) * 0x40 + O - 0x80) *
    0x40 + P - 0x80 else return "" end; if L <= 0xffff then return v("\\u%.4x", L) elseif L <= 0x10ffff then
    L = L - 0x10000; local Q, R = 0xD800 + m(L / 0x400), 0xDC00 + L % 0x400; return v("\\u%.4x\\u%.4x", Q, R)
  else return "" end
end; local function S(T, U, V) if t(T, U) then return p(T, U, V) else return T end end; local function W(L)
  L = S(L, "[%z\1-\31\"\\\127]", J)
  if t(L, "[\194\216\220\225\226\239]") then
    L = S(L, "\194[\128-\159\173]", J)
    L = S(L, "\216[\128-\132]", J)
    L = S(L, "\220\143", J)
    L = S(L, "\225\158[\180\181]", J)
    L = S(L, "\226\128[\140-\143\168-\175]", J)
    L = S(L, "\226\129[\160-\175]", J)
    L = S(L, "\239\187\191", J)
    L = S(L, "\239\191[\176-\191]", J)
  end; return "\"" .. L .. "\""
end; y.quotestring = W; local function X(T, Y, E)
  local Z, _ = t(T, Y, 1, true)
  if Z then return q(T, 1, Z - 1) .. E .. q(T, _ + 1, -1) else return T end
end; local a0, a1; local function a2()
  a0 = w(tostring(0.5), "([^05+])")
  a1 = "[^0-9%-%+eE" .. p(a0, "[%^%$%(%)%%%.%[%]%*%+%-%?]", "%%%0") .. "]+"
end; a2()
local function a3(a4) return X(S(tostring(a4), a1, ""), a0, ".") end; local function a5(T)
  local a4 = tonumber(X(T, ".", a0))
  if not a4 then
    a2()
    a4 = tonumber(X(T, ".", a0))
  end; return a4
end; local function a6(a7, a8, a9)
  a8[a9 + 1] = "\n"
  a8[a9 + 2] = o("  ", a7)
  a9 = a9 + 2; return a9
end; function y.addnewline(aa) if aa.indent then aa.bufferlen = a6(aa.level or 0, aa.buffer, aa.bufferlen or #aa.buffer) end end; local ab; local function ac(
    ad, L, ae, af, a7, a8, a9, ag, ah, aa)
  local ai = type(ad)
  if ai ~= 'string' and ai ~= 'number' then return nil, "type '" .. ai .. "' is not supported as a key by JSON." end; if ae then
    a9 = a9 + 1; a8[a9] = ","
  end; if af then a9 = a6(a7, a8, a9) end; a8[a9 + 1] = W(ad)
  a8[a9 + 2] = ":"
  return ab(L, af, a7, a8, a9 + 2, ag, ah, aa)
end; local function aj(i, a8, aa)
  local a9 = aa.bufferlen; if type(i) == 'string' then
    a9 = a9 + 1; a8[a9] = i
  end; return a9
end; local function ak(al, L, aa, a8, a9, am)
  am = am or al; local an = aa.exception; if not an then return nil, am else
    aa.bufferlen = a9; local ao, ap = an(al, L, aa, am)
    if not ao then return nil, ap or am end; return aj(ao, a8, aa)
  end
end; function y.encodeexception(al, L, aa, am) return W("<" .. am .. ">") end; ab = function(L, af, a7, a8, a9, ag, ah,
                                                                                             aa)
  local aq = type(L)
  local ar = getmetatable(L)
  ar = type(ar) == 'table' and ar; local as = ar and ar.__tojson; if as then
    if ag[L] then return ak('reference cycle', L, aa, a8, a9) end; ag[L] = true; aa.bufferlen = a9; local ao, ap = as(L,
      aa)
    if not ao then return ak('custom encoder failed', L, aa, a8, a9, ap) end; ag[L] = nil; a9 = aj(ao, a8, aa)
  elseif L == nil then
    a9 = a9 + 1; a8[a9] = "null"
  elseif aq == 'number' then
    local at; if L ~= L or L >= n or -L >= n then at = "null" else at = a3(L) end; a9 = a9 + 1; a8[a9] = at
  elseif aq == 'boolean' then
    a9 = a9 + 1; a8[a9] = L and "true" or "false"
  elseif aq == 'string' then
    a9 = a9 + 1; a8[a9] = W(L)
  elseif aq == 'table' then
    if ag[L] then return ak('reference cycle', L, aa, a8, a9) end; ag[L] = true; a7 = a7 + 1; local au, E = B(L)
    if E == 0 and ar and ar.__jsontype == 'object' then au = false end; local ap; if au then
      a9 = a9 + 1; a8[a9] = "["
      for Z = 1, E do
        a9, ap = ab(L[Z], af, a7, a8, a9, ag, ah, aa)
        if not a9 then return nil, ap end; if Z < E then
          a9 = a9 + 1; a8[a9] = ","
        end
      end; a9 = a9 + 1; a8[a9] = "]"
    else
      local ae = false; a9 = a9 + 1; a8[a9] = "{"
      local av = ar and ar.__jsonorder or ah; if av then
        local aw = {}
        E = #av; for Z = 1, E do
          local G = av[Z]
          local H = L[G]
          if H ~= nil then
            aw[G] = true; a9, ap = ac(G, H, ae, af, a7, a8, a9, ag, ah, aa)
            if not a9 then return nil, ap end; ae = true
          end
        end; for G, H in pairs(L) do if not aw[G] then
            a9, ap = ac(G, H, ae, af, a7, a8, a9, ag, ah, aa)
            if not a9 then return nil, ap end; ae = true
          end end
      else for G, H in pairs(L) do
          a9, ap = ac(G, H, ae, af, a7, a8, a9, ag, ah, aa)
          if not a9 then return nil, ap end; ae = true
        end end; if af then a9 = a6(a7 - 1, a8, a9) end; a9 = a9 + 1; a8[a9] = "}"
    end; ag[L] = nil
  else return ak('unsupported type', L, aa, a8, a9, "type '" .. aq .. "' is not supported by JSON.") end; return a9
end; function y.encode(L, aa)
  aa = aa or {}
  local ax = aa.buffer; local a8 = ax or {}
  aa.buffer = a8; a2()
  local ao, ap = ab(L, aa.indent, aa.level or 0, a8, aa.bufferlen or 0, aa.tables or {}, aa.keyorder, aa)
  if not ao then error(ap, 2) elseif ax == a8 then
    aa.bufferlen = ao; return true
  else
    aa.bufferlen = nil; aa.buffer = nil; return x(a8)
  end
end; local function ay(T, az)
  local aA, aB, aC = 1, 1, 0; while true do
    aB = t(T, "\n", aB, true)
    if aB and aB < az then
      aA = aA + 1; aC = aB; aB = aB + 1
    else break end
  end; return v("line %d, column %d", aA, az - aC)
end; local function aD(T, aE, az) return nil, u(T) + 1, "unterminated " .. aE .. " at " .. ay(T, az) end; local function aF(
    T, aB) while true do
    aB = t(T, "%S", aB)
    if not aB then return nil end; local aG = q(T, aB, aB + 1)
    if aG == "\239\187" and q(T, aB + 2, aB + 2) == "\191" then aB = aB + 3 elseif aG == "//" then
      aB = t(T, "[\n\r]", aB + 2)
      if not aB then return nil end
    elseif aG == "/*" then
      aB = t(T, "*/", aB + 2)
      if not aB then return nil end; aB = aB + 2
    else return aB end
  end end; local aH = { ["\""] = "\"", ["\\"] = "\\", ["/"] = "/", ["b"] = "\b", ["f"] = "\f", ["n"] = "\n", ["r"] = "\r",
  ["t"] = "\t" }
local function aI(L) if L < 0 then return nil elseif L <= 0x007f then return s(L) elseif L <= 0x07ff then return s(
    0xc0 + m(L / 0x40), 0x80 + m(L) % 0x40) elseif L <= 0xffff then return s(0xe0 + m(L / 0x1000), 0x80 + m(L / 0x40) %
    0x40, 0x80 + m(L) % 0x40) elseif L <= 0x10ffff then return s(0xf0 + m(L / 0x40000), 0x80 + m(L / 0x1000) % 0x40,
      0x80 + m(L / 0x40) % 0x40, 0x80 + m(L) % 0x40) else return nil end end; local function aJ(T, aB)
  local aK = aB + 1; local a8, E = {}, 0; while true do
    local aL = t(T, "[\"\\]", aK)
    if not aL then return aD(T, "string", aB) end; if aL > aK then
      E = E + 1; a8[E] = q(T, aK, aL - 1)
    end; if q(T, aL, aL) == "\"" then
      aK = aL + 1; break
    else
      local aM = q(T, aL + 1, aL + 1)
      local L; if aM == "u" then
        L = tonumber(q(T, aL + 2, aL + 5), 16)
        if L then
          local aN; if 0xD800 <= L and L <= 0xDBff then if q(T, aL + 6, aL + 7) == "\\u" then
              aN = tonumber(q(T, aL + 8, aL + 11), 16)
              if aN and 0xDC00 <= aN and aN <= 0xDFFF then L = (L - 0xD800) * 0x400 + aN - 0xDC00 + 0x10000 else aN = nil end
            end end; L = L and aI(L)
          if L then if aN then aK = aL + 12 else aK = aL + 6 end end
        end
      end; if not L then
        L = aH[aM] or aM; aK = aL + 2
      end; E = E + 1; a8[E] = L
    end
  end; if E == 1 then return a8[1], aK elseif E > 1 then return x(a8), aK else return "", aK end
end; local aO; local function aP(aE, aQ, T, aR, aS, aT, aU)
  local C, E = {}, 0; local aB = aR + 1; if aE == 'object' then setmetatable(C, aT) else setmetatable(C, aU) end; while true do
    aB = aF(T, aB)
    if not aB then return aD(T, aE, aR) end; local aV = q(T, aB, aB)
    if aV == aQ then return C, aB + 1 end; local aW, aX; aW, aB, aX = aO(T, aB, aS, aT, aU)
    if aX then return nil, aB, aX end; aB = aF(T, aB)
    if not aB then return aD(T, aE, aR) end; aV = q(T, aB, aB)
    if aV == ":" then
      if aW == nil then return nil, aB, "cannot use nil as table index (at " .. ay(T, aB) .. ")" end; aB = aF(T, aB + 1)
      if not aB then return aD(T, aE, aR) end; local aY; aY, aB, aX = aO(T, aB, aS, aT, aU)
      if aX then return nil, aB, aX end; C[aW] = aY; aB = aF(T, aB)
      if not aB then return aD(T, aE, aR) end; aV = q(T, aB, aB)
    else
      E = E + 1; C[E] = aW
    end; if aV == "," then aB = aB + 1 end
  end
end; aO = function(T, aB, aS, aT, aU)
  aB = aB or 1; aB = aF(T, aB)
  if not aB then return nil, u(T) + 1, "no valid JSON value (reached the end)" end; local aV = q(T, aB, aB)
  if aV == "{" then return aP('object', "}", T, aB, aS, aT, aU) elseif aV == "[" then return aP('array', "]", T, aB, aS,
      aT, aU) elseif aV == "\"" then return aJ(T, aB) else
    local aZ, a_ = t(T, "^%-?[%d%.]+[eE]?[%+%-]?%d*", aB)
    if aZ then
      local b0 = a5(q(T, aZ, a_))
      if b0 then return b0, a_ + 1 end
    end; aZ, a_ = t(T, "^%a%w*", aB)
    if aZ then
      local b1 = q(T, aZ, a_)
      if b1 == "true" then return true, a_ + 1 elseif b1 == "false" then return false, a_ + 1 elseif b1 == "null" then return
        aS, a_ + 1 end
    end; return nil, aB, "no valid JSON value at " .. ay(T, aB)
  end
end; local function b2(...) if select("#", ...) > 0 then return ... else return { __jsontype = 'object' },
        { __jsontype = 'array' } end end; function y.decode(T, aB, aS, ...)
  local aT, aU = b2(...)
  return aO(T, aB, aS, aT, aU)
end; function y.use_lpeg()
  local b3 = require("lpeg")
  if type(b3.version) == 'function' and b3.version() == "0.11" then error "due to a bug in LPeg 0.11, it cannot be used for JSON matching" end; local b4 =
  b3.match; local b5, b6, b7 = b3.P, b3.S, b3.R; local function b8(T, aB, ap, aa)
    if not aa.msg then
      aa.msg = ap .. " at " .. ay(T, aB)
      aa.pos = aB
    end; return false
  end; local function b9(ap) return b3.Cmt(b3.Cc(ap) * b3.Carg(2), b8) end; local function ba(T, aB, aE, aa) return b8(T,
      aB - 1, "unterminated " .. aE, aa) end; local bb = b5 "//" * (1 - b6 "\n\r") ^ 0; local bc = b5 "/*" * (1 - b5 "*/") ^
  0 * b5 "*/"
  local bd = (b6 " \n\r\t" + b5 "\239\187\191" + bb + bc) ^ 0; local function be(aE) return b3.Cmt(b3.Cc(aE) * b3.Carg(2),
      ba) end; local bf = 1 - b6 "\"\\\n\r"
  local bg = b5 "\\" * b3.C(b6 "\"\\/bfnrt" + b9 "unsupported escape sequence") / aH; local bh = b7("09", "af", "AF")
  local function bi(bj, aB, bk, bl)
    bk, bl = tonumber(bk, 16), tonumber(bl, 16)
    if 0xD800 <= bk and bk <= 0xDBff and 0xDC00 <= bl and bl <= 0xDFFF then return true,
          aI((bk - 0xD800) * 0x400 + bl - 0xDC00 + 0x10000) else return false end
  end; local function bm(bn) return aI(tonumber(bn, 16)) end; local bo = b5 "\\u" * b3.C(bh * bh * bh * bh)
  local bp = b3.Cmt(bo * bo, bi) + bo / bm; local bq = bp + bg + bf; local br = b5 "\"" * (b3.Cs(bq ^ 0) * b5 "\"" + be "string")
  local bs = b5 "-" ^ -1 * (b5 "0" + b7 "19" * b7 "09" ^ 0)
  local bt = b5 "." * b7 "09" ^ 0; local bu = b6 "eE" * b6 "+-" ^ -1 * b7 "09" ^ 1; local bv = bs * bt ^ -1 * bu ^ -1 /
  a5; local bw = b5 "true" * b3.Cc(true) + b5 "false" * b3.Cc(false) + b5 "null" * b3.Carg(1)
  local bx = bv + br + bw; local by, bz; local function bA(T, aB, aS, aa)
    local bB, bC; local bD = aB; local bE; local bF, bG = {}, 0; repeat
      bB, bC, bE = b4(by, T, aB, aS, aa)
      if bC == 'end' then return ba(T, bD, "array", aa) end; aB = bE; if bC == 'cont' or bC == 'last' then
        bG = bG + 1; bF[bG] = bB
      end
    until bC ~= 'cont'
    return aB, setmetatable(bF, aa.arraymeta)
  end; local function bH(T, aB, aS, aa)
    local bB, ad, bC; local bD = aB; local bE; local bF = {}
    repeat
      ad, bB, bC, bE = b4(bz, T, aB, aS, aa)
      if bC == 'end' then return ba(T, bD, "object", aa) end; aB = bE; if bC == 'cont' or bC == 'last' then bF[ad] = bB end
    until bC ~= 'cont'
    return aB, setmetatable(bF, aa.objectmeta)
  end; local bI = b5 "[" * b3.Cmt(b3.Carg(1) * b3.Carg(2), bA)
  local bJ = b5 "{" * b3.Cmt(b3.Carg(1) * b3.Carg(2), bH)
  local bK = bd * (bI + bJ + bx)
  local bL = bK + bd * b9 "value expected"
  local bM = br + b9 "key expected"
  local bN = b5(-1) * b3.Cc 'end'
  local bO = b9 "invalid JSON"
  by = (bK * bd * (b5 "," * b3.Cc 'cont' + b5 "]" * b3.Cc 'last' + bN + bO) + b3.Cc(nil) * (b5 "]" * b3.Cc 'empty' + bN + bO)) *
  b3.Cp()
  local bP = b3.Cg(bd * bM * bd * (b5 ":" + b9 "colon expected") * bL)
  bz = (b3.Cc(nil) * b3.Cc(nil) * b5 "}" * b3.Cc 'empty' + bN + bP * bd * (b5 "," * b3.Cc 'cont' + b5 "}" * b3.Cc 'last' + bN + bO) + bO) *
  b3.Cp()
  local bQ = bL * b3.Cp()
  z.version = y.version; z.encode = y.encode; z.null = y.null; z.quotestring = y.quotestring; z.addnewline = y
  .addnewline; z.encodeexception = y.encodeexception; z.using_lpeg = true; function z.decode(T, aB, aS, ...)
    local aa = {}
    aa.objectmeta, aa.arraymeta = b2(...)
    local bB, bR = b4(bQ, T, aB, aS, aa)
    if aa.msg then return nil, aa.pos, aa.msg else return bB, bR end
  end; y.use_lpeg = function() return z end; z.use_lpeg = y.use_lpeg; return z
end; if j then return y.use_lpeg() end; function print_nicely(C)
  local bS = 0; local bT = 0; for ad, L in pairs(C) do
    local bU = #tostring(ad)
    local bV = #tostring(L)
    if bU > bS then bS = bU end; if bV > bT then bT = bV end
  end; print("+" .. string.rep("-", bS + 2) .. "+" .. string.rep("-", bT + 2) .. "+")
  print("| " .. "Key" .. string.rep(" ", bS - 3) .. " | " .. "Value" .. string.rep(" ", bT - 5) .. " |")
  print("+" .. string.rep("-", bS + 2) .. "+" .. string.rep("-", bT + 2) .. "+")
  for ad, L in pairs(C) do
    local bW = tostring(ad)
    local bX = tostring(L)
    print("| " .. bW .. string.rep(" ", bS - #bW) .. " | " .. bX .. string.rep(" ", bT - #bX) .. " |")
  end; print("+" .. string.rep("-", bS + 2) .. "+" .. string.rep("-", bT + 2) .. "+")
end; function b:new(bY, bZ)
  local self = setmetatable({}, b)
  self.host = bY or "localhost"
  self.port = bZ or 3000; self.server = a.bind(self.host, self.port)
  self.server:settimeout(nil)
  print("Server listening on " .. self.host .. ":" .. self.port)
  return self
end; function b:accept()
  local b_ = self.server:accept()
  if b_ then
    print("Client connected")
    b_:settimeout(nil)
    return e:fromClientSocket(b_)
  end
end; function b:settimeout(c0) if self.client then self.client:settimeout(c0) end end; function b:setBlocking(c1) if c1 == true then
    self.client:settimeout(nil) else self.client:settimeout(c1) end end; function b:send(c2) if self.client then self
        .client:send(c2) end end; function b:receive(U) if self.client then
    local c2, aX = self.client:receive(U)
    if not c2 then print("Error receiving data:", aX) else print("Received data:", c2) end; return c2
  end end; function Luadap:close()
  if self.client then self.client:close() end; if self.server then self.server:close() end
end; function e:new()
  local self = setmetatable({}, e)
  self.client = a.tcp()
  return self
end; function e:fromClientSocket(b_)
  local self = setmetatable({}, e)
  self.client = b_; self.initialized = false; self.hitBreakpoint = false; self.sendEntryEvent = false; self.hasStartReturned = false; self.stackLevel = 0; self.variablesCount = 0; self.seenFrames = {}
  self.variables = {}
  self.watch = {}
  self.variablestranslation = {}
  self.children = {}
  self.next = false; self.nextStackLevelFirst = true; self.nextStackLevel = 0; self.stepIn = false; self.scopeOffset = 0; self.breakPoints = {}
  self.breakPointsCount = 0; return self
end; function e:connect(bY, bZ)
  local c3, aX = self.client:connect(bY or "localhost", bZ or 3000)
  if not c3 then print("Error connecting:", aX) else print("Connected to", bY or "localhost", "on port", bZ or 3000) end
end; function e:send(c2) self.client:send(c2) end; function e:receive(U)
  local c2, aX = self.client:receive(U)
  if not c2 then print("Error receiving data:", aX) else print("Received data:", c2) end; return c2
end; function e:receivePackage() if self.client then
    local c4 = {}
    while true do
      local aA, aX = self.client:receive("*l")
      if not aA or aA == "" then break else table.insert(c4, aA) end
    end; local c5 = 0; for c6, aA in ipairs(c4) do
      local ad, L = aA:match("^(.-):%s*(.*)$")
      if ad and ad:lower() == "content-length" then
        c5 = tonumber(L)
        break
      end
    end; if c5 > 0 then
      local c7 = 0; local c2 = {}
      while c7 < c5 do
        local c8, aX, c9 = self.client:receive(math.min(1024, c5 - c7))
        if c8 then
          table.insert(c2, c8)
          c7 = c7 + #c8
        elseif c9 then
          table.insert(c2, c9)
          c7 = c7 + #c9
        else
          print("Error receiving data:", aX)
          break
        end
      end; return { headers = c4, body = y.decode(table.concat(c2)) }
    else print("Content-Length not found in header") end
  else print("no client") end end; function e:receivePackageNonBlocking() if self.client then
    local c4 = {}
    local aA, aX = self.client:receive("*l")
    if not aA then return nil end; table.insert(c4, aA)
    while true do
      aA, aX = self.client:receive("*l")
      if not aA or aA == "" then break else table.insert(c4, aA) end
    end; local c5 = 0; for c6, aA in ipairs(c4) do
      local ad, L = aA:match("^(.-):%s*(.*)$")
      if ad and ad:lower() == "content-length" then
        c5 = tonumber(L)
        break
      end
    end; if c5 > 0 then
      local c7 = 0; local c2 = {}
      while c7 < c5 do
        local c8, aX, c9 = self.client:receive(math.min(1024, c5 - c7))
        if c8 then
          table.insert(c2, c8)
          c7 = c7 + #c8
        elseif c9 then
          table.insert(c2, c9)
          c7 = c7 + #c9
        else
          print("Error receiving data:", aX)
          break
        end
      end; return { headers = c4, body = y.decode(table.concat(c2)) }
    else return nil end
  else
    print("no client")
    return nil
  end end; function e:sendPackage(ca) if self.client then
    local cb = y.encode(ca)
    local c5 = #cb; print(cb)
    local c4 = "Content-Length: " .. c5 .. "\r\n\r\n"
    local c3, aX; c3, aX = self.client:send(c4)
    if not c3 then
      print("Error sending header:", aX)
      return nil
    end; c3, aX = self.client:send(cb)
    if not c3 then
      print("Error sending data:", aX)
      return nil
    end; print("Package sent successfully")
  else print("No client connected") end end; function e:close() self.client:close() end; function e:settimeout(c0) self
      .client:settimeout(c0) end; function e:handleInitRequest(cc) if cc.body.command == "initialize" then
    if cc.body.arguments.adapterID ~= "luadap" then
      print("client does not have correct adapterID, expected luadap but was: " .. cc.body.arguments.adapterID)
      return nil
    end; local cd = { clientID = cc.body.arguments.clientID, clientName = cc.body.arguments.clientName, adapterID = cc
    .body.arguments.adapterID }
    print_nicely(cd)
    local ce = { supportsConfigurationDoneRequest = true, supportsEvaluateForHovers = true, supportsLogPoints = true }
    return InitializeResponse:new(cc.body.seq, cc.body.seq, true, "", ce)
  end end; function e:send_event(cf, cg) if cf == "initialize" then
    self:sendPackage(Event:new(cg, "initialized"))
    d.initialized = true
  end end; function e:handleAttach(ch)
  self.sessionInfo = {}
  self.sessionInfo.sessionId = ch.arguments.__sessionId; self.sessionInfo.name = ch.arguments.name; self.sessionInfo.type =
  ch.arguments.type; self.sessionInfo.host = ch.arguments.host; self.sessionInfo.port = ch.arguments.port; self.sessionInfo.cwd =
  ch.arguments.cwd; print(self.sessionInfo.cwd)
  if self.sessionInfo.type ~= "luadap" then
    local ci = { id = 1, format = "Expected type 'luadap'", sendTelemetry = true, showUser = true }
    local cj = ErrorResponse:new(2, 2, false, "attach", "Invalid type", ci)
    print_nicely(cj)
    return cj
  end; return true
end; function e:debugLoop(ck, aA)
  local cc = self:receivePackageNonBlocking()
  if cc ~= nil then
    print_nicely(cc.body)
    local cl = self:handleRequest(cc)
    self:sendPackage(cl)
  end
end; function Luadap.start(bY, bZ)
  c = b:new(bY, bZ)
  print("lua Debug Adapter Server waiting for client to connect!")
  d = c:accept()
  print("client connected, waiting for initialize request")
  local cm = 0; while not d.initialized do
    local cc = d:receivePackage()
    print_nicely(cc.body)
    local cl = d:handleInitRequest(cc)
    print_nicely(cl)
    cm = cc.body.seq; d:send_event(cc.body.command, cc.body.seq)
    d:sendPackage(cl)
  end; print("debugger initialized")
  f.sethook(Luadap.debughook, "lcr")
end; Thread = {}
Thread.__index = Thread; function Thread:new(cn, b1)
  local co = setmetatable({}, self)
  co.id = cn or 0; co.name = b1 or ""
  return co
end; function Thread:display()
  print("Thread ID: " .. self.id)
  print("Thread Name: " .. self.name)
end; ProtocolMessage = {}
ProtocolMessage.__index = ProtocolMessage; function ProtocolMessage:new(cg, type)
  local co = setmetatable({}, self)
  co.seq = cg or 1; co.type = type or 'request'
  return co
end; function ProtocolMessage:display()
  print("Sequence Number: " .. self.seq)
  print("Type: " .. self.type)
end; Request = setmetatable({}, { __index = ProtocolMessage })
Request.__index = Request; function Request:new(cg, cf, cp)
  local co = ProtocolMessage.new(self, cg, 'request')
  co.command = cf or ""
  co.arguments = cp or {}
  return co
end; function Request:display()
  ProtocolMessage.display(self)
  print("Command: " .. self.command)
  print("Arguments: " .. tostring(self.arguments))
end; Event = setmetatable({}, { __index = ProtocolMessage })
Event.__index = Event; function Event:new(cg, ck, cq)
  local co = ProtocolMessage.new(self, cg, 'event')
  co.event = ck; co.body = cq; return co
end; function Event:display()
  ProtocolMessage.display(self)
  print("Event: " .. self.event)
  print("Body: " .. tostring(self.body))
end; ThreadEvent = setmetatable({}, { __index = Event })
ThreadEvent.__index = ThreadEvent; function ThreadEvent:new(cg, al, cr)
  local cq = { reason = al or "", threadId = cr or 0 }
  local co = Event.new(self, cg, 'thread', cq)
  return co
end; function ThreadEvent:display()
  Event.display(self)
  print("Reason: " .. self.body.reason)
  print("Thread ID: " .. self.body.threadId)
end; StoppedEvent = setmetatable({}, { __index = Event })
StoppedEvent.__index = StoppedEvent; function StoppedEvent:new(cg, al, cr, cs, ct)
  local co = Event.new(self, cg, 'stopped', { reason = al, threadId = cr, allThreadsStopped = cs, hitBreakpointIds = ct })
  return co
end; ContinuedEvent = setmetatable({}, { __index = Event })
ContinuedEvent.__index = StoppedEvent; function ContinuedEvent:new(cg, al, cr, cu, ct)
  local co = Event.new(self, cg, 'continued', { threadId = cr, allThreadsContinued = cu })
  return co
end; Response = setmetatable({}, { __index = ProtocolMessage })
Response.__index = Response; function Response:new(cg, cv, c3, cf, cw, cq)
  local co = ProtocolMessage.new(self, cg, 'response')
  co.request_seq = cv or 1; co.success = c3 or false; co.command = cf; co.message = cw; co.body = cq; return co
end; function Response:display()
  ProtocolMessage.display(self)
  print("Request Sequence Number: " .. self.request_seq)
  print("Success: " .. tostring(self.success))
  print("Command: " .. self.command)
  print("Message: " .. self.message)
  print("Body: " .. tostring(self.body))
end; InitializeResponse = setmetatable({}, { __index = Response })
InitializeResponse.__index = InitializeResponse; function InitializeResponse:new(cg, cv, c3, cw, ce)
  cg = cg or 1; cv = cv or 1; c3 = c3; cw = cw; ce = ce or {}
  local co = Response.new(self, cg, cv, c3, "initialize", cw, ce)
  return co
end; ErrorResponse = setmetatable({}, { __index = Response })
ErrorResponse.__index = ErrorResponse; function ErrorResponse:new(cg, cv, c3, cf, cw, error)
  local cx = Message:new(error.id, error.format, error.variables, error.sendTelemetry, error.showUser, error.url,
    error.urlLabel)
  local cq = { error = cx }
  local co = Response.new(self, cg, cv, c3, cf, cw, cq)
  return co
end; function ErrorResponse:display()
  Response.display(self)
  print("Error: " .. tostring(self.body.error))
end; CancelRequest = setmetatable({}, { __index = Request })
CancelRequest.__index = CancelRequest; function CancelRequest:new(cg, cp)
  local co = Request.new(self, cg, 'cancel', cp)
  return co
end; function CancelRequest:display() Request.display(self) end; CancelResponse = setmetatable({}, { __index = Response })
CancelResponse.__index = CancelResponse; function CancelResponse:new(cg, cv, c3, cf, cw)
  local co = Response.new(self, cg, cv, c3, cf, cw, nil)
  return co
end; function CancelResponse:display() Response.display(self) end; Message = {}
Message.__index = Message; function Message:new(cn, cy, cz, cA, cB, cC, cD)
  local co = setmetatable({}, Message)
  co.id = cn; co.format = cy; co.variables = cz; co.sendTelemetry = cA or false; co.showUser = cB or false; co.url = cC; co.urlLabel =
  cC; return co
end; function Message:display()
  print("ID: " .. self.id)
  print("Format: " .. self.format)
  print("Variables: " .. tostring(self.variables))
  print("Send Telemetry: " .. tostring(self.sendTelemetry))
  print("Show User: " .. tostring(self.showUser))
  print("URL: " .. self.url)
  print("URL Label: " .. self.urlLabel)
end; AttachResponse = setmetatable({}, { __index = Response })
AttachResponse.__index = AttachResponse; function AttachResponse:new(cg, cv, c3, cw)
  local co = Response.new(self, cg, cv, c3, "attach", cw)
  return co
end; ConfigurationDoneResponse = setmetatable({}, { __index = Response })
ConfigurationDoneResponse.__index = AttachResponse; function ConfigurationDoneResponse:new(cg, cv, c3, cw)
  local co = Response.new(self, cg, cv, c3, "configurationDone", cw)
  return co
end; SetExceptionBreakpointsResponse = setmetatable({}, { __index = Response })
SetExceptionBreakpointsResponse.__index = SetExceptionBreakpointsResponse; function SetExceptionBreakpointsResponse:new(
    cg, cv, c3, cw)
  local co = Response.new(self, cg, cv, c3, "setExceptionBreakpoints", cw, nil)
  return co
end; ThreadsResponse = setmetatable({}, { __index = Response })
ThreadsResponse.__index = ThreadsResponse; function ThreadsResponse:new(cg, cv, c3, cE, cw)
  local cq = { threads = cE or {} }
  local co = Response.new(self, cg, cv, c3, "threads", cw, cq)
  return co
end; function ThreadsResponse:display()
  Response.display(self)
  if self.body.threads then for c6, cF in ipairs(self.body.threads) do cF:display() end end
end; Source = {}
Source.__index = Source; function Source:new(b1, cG)
  local co = setmetatable({}, Source)
  co.name = b1 or nil; co.path = cG or nil; co.sourceReference = 0; return co
end; function Source:display() print(string.format("Source Name: %s | Path: %s", self.name or "[none]",
    self.path or "[unknown]")) end; StackTraceResponse = setmetatable({}, { __index = Response })
StackTraceResponse.__index = StackTraceResponse; function StackTraceResponse:new(cg, cv, c3, cH, cw)
  local cq = { stackFrames = cH or {} }
  local co = Response.new(self, cg, cv, c3, "stackTrace", cw, cq)
  return co
end; function StackTraceResponse:display()
  Response.display(self)
  if self.body.stackFrames then for c6, cI in ipairs(self.body.stackFrames) do cI:display() end end
end; StackFrame = {}
StackFrame.__index = StackFrame; function StackFrame:new(cn, b1, cJ, aA, cK)
  local co = setmetatable({}, StackFrame)
  co.id = cn; co.name = b1 or "[anonymous]"
  co.source = cJ or Source:new()
  co.line = aA or 0; co.column = cK or 1; co.column = 0; return co
end; function StackFrame:display() print(string.format("Frame ID: %d | Name: %s | Source: %s | Line: %d | Column: %d",
    self.id, self.name, self.source.path, self.line, self.column)) end; VariablePresentationHint = {}
VariablePresentationHint.__index = VariablePresentationHint; function VariablePresentationHint:new(cL, cM, cN, cO)
  local co = setmetatable({}, self)
  co.kind = cL; co.attributes = cM or {}
  co.visibility = cN; co.lazy = cO or false; return co
end; function VariablePresentationHint:display()
  print("Kind: " .. (self.kind or "nil"))
  print("Attributes: " .. table.concat(self.attributes, ", "))
  print("Visibility: " .. (self.visibility or "nil"))
  print("Lazy: " .. tostring(self.lazy))
end; Variable = {}
Variable.__index = Variable; function Variable:new(b1, L, cP, cQ, cR)
  local co = setmetatable({}, self)
  co.name = b1 or ""
  co.value = L or ""
  co.variablesReference = cP or 0; co.presentationHint = cQ; co.evaluateName = cR; if type(L) == "table" then
    co.indexedVariables = #L; local cS = 0; for G in pairs(L) do if type(G) ~= "number" or G % 1 ~= 0 or G <= 0 or G > co.indexedVariables then cS =
        cS + 1 end end; co.namedVariables = cS > 0 and cS or nil
  else
    co.namedVariables = 0; co.indexedVariables = 0
  end; if cQ.kind == "property" then co.variablesReference = 0 end; local cT = getmetatable(co) or {}
  cT.value = L or ""
  setmetatable(co, cT)
  return co
end; function e:getPresentationHint(L)
  local cU = type(L)
  local cL = nil; if cU == "function" then cL = "method" elseif cU == "userdata" then cL = "class" elseif cU == "table" then if getmetatable(L) then cL =
      "class" else cL = "data" end elseif cU == "thread" then cL = "event" elseif cU == "boolean" or cU == "number" or cU == "string" then cL =
    "property" end; return VariablePresentationHint:new(cL, {}, nil, false)
end; function Variable:display()
  print("Name: " .. self.name)
  print("Value: " .. self.value)
  print("Variables Reference: " .. self.variablesReference)
  if self.presentationHint then print("Presentation Hint: " .. tostring(self.presentationHint)) end; if self.evaluateName then
    print("Evaluate Name: " .. self.evaluateName) end; if self.namedVariables then print("Named Variables: " ..
    self.namedVariables) end; if self.indexedVariables then print("Indexed Variables: " .. self.indexedVariables) end
end; Scope = {}
Scope.__index = Scope; function Scope:new(b1, cP, cV, cQ, cW, cX)
  local co = setmetatable({}, self)
  co.name = b1 or ""
  co.presentationHint = cQ; co.variablesReference = cP or 0; co.namedVariables = cW; co.indexedVariables = cX; co.expensive =
  cV or false; return co
end; function Scope:display()
  print("Name: " .. self.name)
  print("Presentation Hint: " .. (self.presentationHint or "nil"))
  print("Variables Reference: " .. self.variablesReference)
  if self.namedVariables then print("Named Variables: " .. self.namedVariables) end; if self.indexedVariables then print(
    "Indexed Variables: " .. self.indexedVariables) end; print("Expensive: " .. tostring(self.expensive))
end; ScopesResponse = setmetatable({}, { __index = Response })
ScopesResponse.__index = ScopesResponse; function ScopesResponse:new(cg, cv, c3, cf, cw, cY)
  local co = Response.new(self, cg, cv, c3, cf, cw, { scopes = cY or {} })
  setmetatable(co, self)
  return co
end; function ScopesResponse:display()
  Response.display(self)
  print("Scopes:")
  if #self.body.scopes == 0 then print("No scopes available.") else for cZ, c_ in ipairs(self.body.scopes) do
      print("Scope " .. cZ .. ":")
      c_:display()
    end end
end; local function d0(cG)
  cG = cG:gsub("\\", "/")
  if cG:match("^/") or cG:match("^[a-zA-Z]:") then return cG end; local d1; if not d.sessionInfo.cwd then
    local d2 = io.popen("cd")
    d1 = d2:read("*a"):gsub("\n", ""):gsub("\\", "/")
    d2:close()
  else d1 = d.sessionInfo.cwd end; local d3 = {}
  for d4 in (d1 .. "/" .. cG):gmatch("[^/]+") do if d4 == ".." then table.remove(d3) elseif d4 ~= "." then table.insert(
      d3, d4) end end; return "/" .. table.concat(d3, "/")
end; function e:getStackFrames(d5, d6)
  local cH = {}
  local a7 = self.stackLevel + 1; local d7 = 0; local d8 = d6 or 6; while a7 >= -1 and d7 < d5 do
    local d9 = f.getinfo(a7 + d8, "nSl")
    if not d9 then break end; local da = d9.short_src and d9.short_src:gsub("\\", "/") or "[unknown]"
    da = d0(da)
    local db = da:gsub("/C:", "C:", 1):gsub("/", "\\")
    local cJ = Source:new(da:match("[^/\\]+$") or "[unknown]", db)
    local dc = StackFrame:new(a7 + 1, d9.name or cJ.name or "[unknown]", cJ, d9.currentline or 0)
    if dc.source.name == "[C]" then return nil else if dc.currentline ~= -1 and dc.source.name ~= "luadap.lua" then table
            .insert(cH, 1, dc) end end; a7 = a7 - 1; d7 = d7 + 1
  end; return cH
end; StackTraceResponse = setmetatable({}, { __index = Response })
StackTraceResponse.__index = StackTraceResponse; function StackTraceResponse:new(cg, cv, c3, cH, dd, cw)
  local cq = { stackFrames = cH or {}, totalFrames = dd }
  local co = Response.new(self, cg, cv, c3, "stackTrace", cw, cq)
  return co
end; function StackTraceResponse:display()
  Response.display(self)
  if self.body.stackFrames then
    print("Stack Frames:")
    for c6, cI in ipairs(self.body.stackFrames) do cI:display() end
  end; if self.body.totalFrames then print("Total Frames: " .. self.body.totalFrames) end
end; VariablesResponse = setmetatable({}, { __index = Response })
VariablesResponse.__index = VariablesResponse; function VariablesResponse:new(cg, cv, c3, cw, cz)
  local co = Response.new(self, cg, cv, c3, "variables", cw, { variables = cz })
  return co
end; NextResponse = setmetatable({}, { __index = Response })
NextResponse.__index = NextResponse; function NextResponse:new(cg, cv, c3, cw, cq) return Response.new(self, cg, cv, c3,
    "next", cw, cq) end; function NextResponse:display() Response.display(self) end; StepInResponse = setmetatable({},
  { __index = Response })
StepInResponse.__index = StepInResponse; function StepInResponse:new(cg, cv, c3, cw, cq) return Response.new(self, cg, cv,
    c3, "stepIn", cw, cq) end; function StepInResponse:display() Response.display(self) end; EvaluateResponse =
setmetatable({}, { __index = Response })
EvaluateResponse.__index = EvaluateResponse; function EvaluateResponse:new(cg, cv, c3, de, cP, df, cQ, cW, cX, dg, dh)
  local cq = { result = de, variablesReference = cP or 0, type = df or nil, presentationHint = cQ or nil, namedVariables =
  cW or nil, indexedVariables = cX or nil, memoryReference = dg or nil, valueLocationReference = dh or nil }
  local co = Response.new(self, cg, cv, c3, "evaluate", nil, cq)
  return co
end; function EvaluateResponse:display()
  Response.display(self)
  print("Result: " .. tostring(self.body.result))
  print("Type: " .. tostring(self.body.type))
  print("Variables Reference: " .. tostring(self.body.variablesReference))
  print("Named Variables: " .. tostring(self.body.namedVariables))
  print("Indexed Variables: " .. tostring(self.body.indexedVariables))
  print("Memory Reference: " .. tostring(self.body.memoryReference))
  print("Value Location Reference: " .. tostring(self.body.valueLocationReference))
end; SetBreakpointsResponse = setmetatable({}, { __index = Response })
SetBreakpointsResponse.__index = SetBreakpointsResponse; function SetBreakpointsResponse:new(cg, cv, c3, di)
  local cq = { breakpoints = di or {} }
  local co = Response.new(self, cg, cv, c3, "setBreakpoints", nil, cq)
  return co
end; function SetBreakpointsResponse:display()
  Response.display(self)
  print("Breakpoints:")
  for Z, dj in ipairs(self.body.breakpoints) do print(string.format(
    "  [%d] verified: %s, line: %s, column: %s, source: %s", Z, tostring(dj.verified), tostring(dj.line),
      tostring(dj.column), dj.source and tostring(dj.source.path) or "nil")) end
end; ContinueResponse = setmetatable({}, { __index = Response })
ContinueResponse.__index = ContinueResponse; function ContinueResponse:new(cg, cv, c3, cu)
  local cq = { allThreadsContinued = cu or true }
  return Response.new(self, cg, cv, c3, "continue", nil, cq)
end; function ContinueResponse:display()
  Response.display(self)
  print("All Threads Continued: " .. tostring(self.body.allThreadsContinued))
end; function e:handleRequest(cc) if cc.body.command == "attach" then
    local dk = self:handleAttach(cc.body)
    if dk == true then return AttachResponse:new(cc.body.seq, cc.body.seq, true) else
      dk.seq = cc.body.seq; dk.request_seq = cc.body.seq; return
    end
  elseif cc.body.command == "setExceptionBreakpoints" then return SetExceptionBreakpointsResponse:new(cc.body.seq,
      cc.body.seq, true) elseif cc.body.command == "threads" then
    local dl = Thread:new(1, "Main Routine")
    d.configurationDone = true; return ThreadsResponse:new(cc.body.seq, cc.body.seq, true, { dl })
  elseif cc.body.command == "configurationDone" then
    d:settimeout(0)
    return ConfigurationDoneResponse:new(cc.body.seq, cc.body.seq, true)
  elseif cc.body.command == "stackTrace" then
    local cH = d:getStackFrames(10)
    return StackTraceResponse:new(cc.body.seq, cc.body.seq, true, cH)
  elseif cc.body.command == "source" then elseif cc.body.command == "scopes" then
    local cH = d:getStackFrames(10)
    d.scopeOffset = 0; local cY = {}
    for c6, dc in ipairs(cH) do
      local c_ = Scope:new(dc.name, dc.id + 1, false, VariablePresentationHint:new("virtual", {}, nil, false))
      table.insert(cY, c_)
      if dc.id + 1 > d.scopeOffset then d.scopeOffset = dc.id + 1 end
    end; return ScopesResponse:new(cc.body.seq, cc.body.seq, true, "scopes", false, cY)
  elseif cc.body.command == "variables" then if cc.body.arguments.variablesReference <= self.scopeOffset then
      local dm = self:getLocalsForFrame(cc.body.arguments.variablesReference)
      return VariablesResponse:new(cc.body.seq, cc.body.seq, true, "Variables", dm)
    else
      local dn = self.variables[cc.body.arguments.variablesReference]
      if dn ~= nil then
        local dp = self.children[cc.body.arguments.variablesReference]
        if dp == nil then dp = self:indexChildren(cc.body.arguments.variablesReference) end; return VariablesResponse
        :new(cc.body.seq, cc.body.seq, true, "Variables", dp)
      end; print("variable not found 404")
    end elseif cc.body.command == "next" then
    d.hitBreakpoint = false; d.variablesCount = 0; d.variables = {}
    d.scopeOffset = 0; d.variablestranslation = {}
    d.next = true; if d.nextStackLevelFirst == true then
      d.nextStackLevelFirst = false; d.nextStackLevel = d.stackLevel - 1
    else d.nextStackLevel = d.stackLevel end; return ContinuedEvent:new(cc.body.seq, "next", 1, true)
  elseif cc.body.command == "stepIn" then
    d.hitBreakpoint = false; d.variablesCount = 0; d.variables = {}
    d.scopeOffset = 0; d.variablestranslation = {}
    d.stepIn = true; return ContinuedEvent:new(cc.body.seq, "next", 1, true)
  elseif cc.body.command == "evaluate" and cc.body.arguments.context == "hover" then
    print_nicely(cc.body.arguments)
    local dq = self.variablestranslation[cc.body.arguments.expression]
    local dn = self.variables[dq]
    if dq then
      local dr = self:getPresentationHint(dn)
      if dr.kind == "data" or dr.kind == "class" then return EvaluateResponse:new(cc.body.seq, cc.body.seq, true,
          tostring(dn), dq) elseif dr.kind == "method" then return EvaluateResponse:new(cc.body.seq, cc.body.seq, true,
          tostring(dn)) else return EvaluateResponse:new(cc.body.seq, cc.body.seq, true, tostring(dn)) end
    else return EvaluateResponse:new(cc.body.seq, cc.body.seq, false, cc.body.arguments.expression, -1, "undefined") end
  elseif cc.body.command == "evaluate" and cc.body.arguments.context == "repl" then
    local ds = { assert = assert, error = error, ipairs = ipairs, next = next, pairs = pairs, pcall = pcall, print =
    print, select = select, tonumber = tonumber, tostring = tostring, type = type, unpack = table.unpack, math = math, string =
    string, table = table }
    local dt = setmetatable({}, { __index = ds })
    for du in string.gmatch(cc.body.arguments.expression, "[a-zA-Z_][a-zA-Z0-9_]*") do
      local dq = self.variablestranslation[du]
      local dn = self.variables[dq]
      dt[du] = dn
    end; local dv = load("return " .. cc.body.arguments.expression, "eval", "t", dt)
    local c3, de = pcall(dv)
    if c3 then return EvaluateResponse:new(cc.body.seq, cc.body.seq, true, tostring(de)) else return EvaluateResponse
      :new(cc.body.seq, cc.body.seq, false, cc.body.arguments.expression, -1, "undefined") end
  elseif cc.body.command == "evaluate" and cc.body.arguments.context == "watch" then
    local error = { id = 1001, format = "Watch expressions are not supported by this adapter.", showUser = true }
    return ErrorResponse:new(cc.body.seq, cc.body.seq, false, "evaluate", "not supported", error)
  elseif cc.body.command == "setBreakpoints" then
    local di = self:indexBreakpoints(cc.body.arguments.source, cc.body.arguments.breakpoints)
    return SetBreakpointsResponse:new(cc.body.seq, cc.body.seq, true, di)
  elseif cc.body.command == "continue" then
    self.hitBreakpoint = false; self.next = false; self.stepIn = false; return ContinueResponse:new(cc.body.seq,
      cc.body.seq, true)
  end end; function e:getFile() return f.getinfo(2, "S").source:sub(2) end; function e:getModule()
  local dw = f.getinfo(3, "S").source; if dw:sub(1, 1) == "@" then return dw:sub(2):match("([^/\\]+)%.lua$") else return
    "[C]" end
end; function e:getModuleName()
  local dw = f.getinfo(3, "S").source; if dw:sub(1, 1) == "@" then return dw:sub(2):match("([^/\\]+%.lua)$") else return
    "[C]" end
end; function e:traceback()
  local a7 = 1; while true do
    local d9 = f.getinfo(a7, "Sl")
    if not d9 then break end; if d9.what == "C" then elseif d9.short_src:match("([^/\\]+)%.lua$") ~= "luadap" then return
      d9.short_src:match("([^/\\]+)%.lua$"), d9.linedefined end; a7 = a7 + 1
  end
end; function e:getLocalsForFrame(dx)
  local dm = {}
  local dy = dx + 4; local cZ = 1; while true do
    local b1, L = f.getlocal(dy, cZ)
    if not b1 then break end; if b1 ~= "(*temporary)" then
      local dq = self.variablesCount + self.scopeOffset + 1; local dr = self:getPresentationHint(L)
      local dz = 0; if dr.kind == "data" or dr.kind == "class" then dz = dq end; self.variables[dq] = L; local dn =
      Variable:new(b1, tostring(L), dz, self:getPresentationHint(L), b1)
      table.insert(dm, dn)
      self.variablestranslation[b1] = dq; self.variablesCount = self.variablesCount + 1
    end; cZ = cZ + 1
  end; return dm
end; function e:indexChildren(cP)
  if cP == 0 then return {} end; local dp = {}
  local L = self.variables[cP]
  if L ~= nil then for ad, dA in pairs(L) do
      local dq = self.variablesCount + self.scopeOffset + 1; local dr = self:getPresentationHint(dA)
      local dz = 0; if dr.kind == "data" or dr.kind == "class" then dz = dq end; if tonumber(ad) then
        local dB = Variable:new(tostring(ad), tostring(dA), dz, dr)
        table.insert(dp, dB)
        self.variables[dq] = dA; self.variablesCount = self.variablesCount + 1
      elseif ad then
        local dB = Variable:new(tostring(ad), tostring(dA), dz, dr)
        table.insert(dp, dB)
        self.variables[dq] = dA; self.variablesCount = self.variablesCount + 1
      else print("oops") end
    end end; self.children[cP] = dp; return dp
end; function e:indexBreakpoints(cJ, di)
  local dC = {}
  for c6, dj in ipairs(di) do
    self.breakPointsCount = self.breakPointsCount + 1; table.insert(dC,
      { line = dj.line, message = dj.logMessage, verified = true, id = self.breakPointsCount })
  end; self.breakPoints[cJ.name] = dC; return dC
end; function Luadap.debughook(ck, aA)
  while not d.configurationDone do d:debugLoop(ck, aA) end; if ck == "call" then d.stackLevel = d.stackLevel + 1 elseif ck == "return" or ck == "tail return" then d.stackLevel =
    d.stackLevel - 1 end; local d9 = f.getinfo(d.stackLevel + 4, "Snl")
  local dD = f.getinfo(d.stackLevel + 3, "Snl")
  local g = d:getModule()
  local dE = d:getModuleName()
  if d.stackLevel >= -1 and d9 ~= nil and d9.currentline ~= -1 and g ~= "[C]" then if ck == "line" and not d.first_line_event and not d9.short_src:match("luadap.lua$") and g == "luadap" then
      d.first_line_event = true; d.hitBreakpoint = true; d:sendPackage(StoppedEvent:new(0, "entry", 1, true))
    end end; if (ck == "line" or ck == "call") and dD ~= nil and d.first_line_event == true then
    if d.stepIn == true and g ~= "[C]" then
      d.hitBreakpoint = true; d.stepIn = false; d:sendPackage(StepInResponse:new(0, 1, true))
      d:sendPackage(StoppedEvent:new(0, "step", 1, true))
    elseif d.next == true and g ~= "[C]" and ck == "line" and d.stackLevel == d.nextStackLevel then
      print_nicely(d9)
      d.hitBreakpoint = true; d.next = false; d:sendPackage(NextResponse:new(0, 1, true))
      d:sendPackage(StoppedEvent:new(0, "step", 1, true))
    end; if d.breakPoints[dE] ~= nil then for c6, dj in ipairs(d.breakPoints[dE]) do if dj.line == aA then
          if dj.message ~= nil then print(dj.message) elseif d.hitBreakpoint == false then
            d.hitBreakpoint = true; d:sendPackage(StoppedEvent:new(0, "breakpoint", 1, true, { dj.id }))
          end; break
        end end end
  elseif ck == "line" and d.stackLevel >= -1 then print("executing line:" .. aA .. " level:" .. d.stackLevel) end; while d.hitBreakpoint do
    d:debugLoop(ck, aA) end; d:debugLoop(ck, aA)
end; return Luadap
