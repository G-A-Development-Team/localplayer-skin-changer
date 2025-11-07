-- Known-good default model to validate pipeline; change back after confirmation

local filename = "models/chicken/chicken.vmdl" -- model file

-- ==========================================================
-- Custom package loader patch (fixed vararg usage)
-- ==========================================================
xpcall(function(...)
    do
        local a = file.Read
        local b = table.concat
        local c = debug.getregistry
        local pcall = pcall
        local error = error
        local load = load
        local select = select
        local type = type
        local unpack = unpack
        local d = debug.getinfo
        local ipairs = ipairs
        local pairs = pairs

        local e = "!\\lua\\"
        local f = b { ".\\?.lua;", e, "?.lua;", e, "?\\init.lua;" }
        local g = "\\"
        local h = ";"
        local i = "?"
        local j = "!"
        local k = "-"
        local l = b({ g, h, i, j, k, "" }, "\n")
        local m = "package"
        local n = c()

        local function dbg(...)
            print("[DEBUG][package]", ...)
        end

        local function o(p)
            dbg("try file.Read", tostring(p))
            return pcall(a, p:gsub("^%.\\", ""))
        end

        local function q(p, r, s)
            local t, u = o(p)
            if not t then
                dbg("file.Read failed", p, u)
                return error(("cannot open %s: %s"):format(p, u:lower()))
            end
            dbg("file.Read success", p)
            local ok, chunk = pcall(load, u, ("=%s"):format(p), r, s)
            if not ok then dbg("load failed for", p, chunk) end
            return ok and chunk or load(u, ("=%s"):format(p), r, s)
        end

        local function v()
            local name = (d(2, "n") and d(2, "n").name) or "?"
            dbg("caller name", name)
            return name or "?"
        end

        local function w(...)
            local x = { ... }
            local y, z, A, B = unpack(x)
            if select("#", ...) < 3 then A, B = ".", g end
            if select("#", ...) < 4 then B = g end
            local C = v()

            if type(y) ~= "string" then
                return error(("bad argument #1 to '%s' (string expected, got %s)"):format(C, select("#", ...) < 1 and "no value" or type(y)))
            end
            if type(z) ~= "string" then
                return error(("bad argument #2 to '%s' (string expected, got %s)"):format(C, select("#", ...) < 2 and "no value" or type(z)))
            end
            if type(A) ~= "string" then
                return error(("bad argument #3 to '%s' (string expected, got %s)"):format(C, select("#", ...) < 3 and "no value" or type(A)))
            end
            if type(B) ~= "string" then
                return error(("bad argument #4 to '%s' (string expected, got %s)"):format(C, select("#", ...) < 4 and "no value" or type(B)))
            end

            local D = {}
            if A then y = y:gsub(("%%%s"):format(A), ("%%%s"):format(B)) end
            dbg("search path", z, "module", y, "sep_replace", A, "with", B)

            for E in z:gmatch(("[^%s]+"):format(h)) do
                local p = E:gsub(("%%%s"):format(i), y)
                dbg("trying path", p)
                if o(p) then
                    dbg("found file at", p)
                    return p
                end
                D[#D + 1] = ("\n\tno file '%s'"):format(p)
            end
            dbg("not found", y)
            return nil, b(D)
        end

        local function F(...)
            local y = unpack { ... }
            if type(y) ~= "string" then
                return error(("bad argument #1 to '%s' (string expected, got %s)"):format(v(), select("#", ...) < 1 and "no value" or type(y)))
            end
            local G = _G[m]["preload"]
            if type(G) ~= "table" then return error("'package.preload' must be a table") end
            if G[y] ~= nil then dbg("found in preload", y) return G[y] end
            dbg("not in preload", y)
            return ("\n\tno field package.preload['%s']"):format(y)
        end

        local function H(...)
            local x = { ... }
            local y = unpack(x)
            if type(y) ~= "string" then
                return error(("bad argument #1 to '%s' (string expected, got %s)"):format(v(), select("#", ...) < 1 and "no value" or type(y)))
            end
            local z = _G[m]["path"]
            if type(z) ~= "string" then return error("'package.path' must be a string") end
            local p, D = w(y, z)
            if not p then dbg("searchpath returned error for", y, D) return D end
            local I, J = xpcall(function() return q(p) end, function(err)
                dbg("error in q:", err, debug.traceback())
                return nil, err
            end)
            if I and J then dbg("loaded module from", p) return J end
            if not I then dbg("error loading module", y, J) end
            return error(("error loading module '%s' from file '%s':\n\t%s"):format(y, p, J))
        end

        local K = bit.bor(bit.lshift(0x80000000, 32), 115)
        local function L(...)
            local y = unpack { ... }
            if type(y) ~= "string" then
                return error(("bad argument #1 to '%s' (string expected, got %s)"):format(v(), select("#", ...) < 1 and "no value" or type(y)))
            end
            local package = _G[m]
            local M = package["loaders"]
            if type(M) ~= "table" then return error("'package.loaders' must be a table") end
            local N = package["loaded"]
            if N[y] then
                if N[y] == K then return error(("loop or previous error loading module '%s'"):format(y)) end
                dbg("module already loaded", y)
                return N[y]
            end
            local D = {}
            for O, P in ipairs(M) do
                dbg("calling loader", O)
                local t, u = pcall(P, y)
                if not t then dbg("loader error", u) return error(u) end
                if type(u) == "function" then
                    dbg("loader returned function for", y)
                    N[y] = K
                    local Q, R = pcall(u, y)
                    if not Q then N[y] = nil; dbg("module init failed", R); return error(R) end
                    N[y] = type(R) == "nil" and true or R
                    dbg("module loaded", y)
                    return N[y]
                elseif type(u) == "string" then
                    D[#D + 1] = u
                end
            end
            dbg("module not found", y)
            return error(("module '%s' not found:%s"):format(y, b(D)))
        end

        local function S()
            dbg("installing compatibility package/require")
            _G[m] = {
                ["searchpath"] = w,
                ["loaders"] = { F, H },
                ["path"] = f:gsub(j, "."),
                ["config"] = l,
                ["loaded"] = n["_LOADED"],
                ["preload"] = n["_PRELOAD"]
            }
            _G["require"] = L
        end

        if not package then S() end
    end
    return package
end, print, function(err)
    print("[DEBUG][package] xpcall outer error:", err, debug.traceback())
end)

-- ==========================================================
-- Rest of your file (FFI setup, hooks, event system, etc.)
-- ==========================================================

-- (Everything below here from your version is correct)
-- ffi, vtable, events, hooks, frame handling, etc.
-- You can safely keep all that unchanged.

-- Path normalization and multi-candidate helpers
local function normalize_vmdl_path(path)
    if not path or type(path) ~= "string" then return path end
    local p = path:gsub("\\", "/"):lower()
    p = p:gsub("^%a:%/", "")
    p = p:gsub("^program files %(x86%)/.-/game/csgo/", "")
    p = p:gsub("^steam/.-/game/csgo/", "")
    p = p:gsub("^.*/csgo/", "")
    return p
end

local function build_candidates(base)
    local t = {}
    local function add(x)
        if x and x ~= "" then
            for _, v in ipairs(t) do if v == x then return end end
            table.insert(t, x)
        end
    end
    -- only use normalized forms and sane roots
    local n = normalize_vmdl_path(base)
    if n and n ~= "" then add(n) end
    if n and not (n:find("^models/") or n:find("^characters/")) then
        add("models/" .. n)
        add("characters/" .. n)
    end
    if n then
        local n2 = n:gsub("^characters/models/", "characters/")
        if n2 ~= n then add(n2) end
    end
    return t
end

local function make_cbuffer_from_string(CBufferString, s)
    local ok, buf = pcall(function()
        return CBufferString(0, bit.bor(1 * 0x80000000, 0x40000000, 8), nil)
    end)
    if not ok or not buf then return nil end
    local ok2 = pcall(function() buf:Insert(0, s, -1, false) end)
    if not ok2 then return nil end
    return buf
end

local ffi = require "ffi"
ffi.cdef[[
    void* GetModuleHandleA(const char* lpModuleName);
    void* GetProcAddress(void* hModule, const char* lpProcName);
]]
local C = ffi.C

-- Debug helpers
local function DbgPrint(...)
    local t = {...}
    for i = 1, #t do t[i] = tostring(t[i]) end
    print("[DEBUG]", table.concat(t, " "))
end

local function safe_xpcall(fn, context)
    return xpcall(fn, function(err)
        print("[ERROR]", context or "anon", err)
        print(debug.traceback())
    end)
end

---@format disable-next
local absolute = (function()local a=ffi.sizeof"int"return function(b,c,d) assert(type(b)=="string") assert(type(c)=="string") assert(type(d)=="number") DbgPrint("absolute: FindPattern", b, c) local e=mem.FindPattern(b,c) if e==nil then DbgPrint("absolute: pattern not found", b, c) return assert(false) end; e=ffi.cast("uintptr_t",e)+d; local adjust = ffi.cast("int32_t*",e)[0]; DbgPrint("absolute: base ptr", tostring(e), "adjust", tostring(adjust)) return e+ffi.cast("int", adjust)+a end end)()

---@format disable-next
local function get_module_base(name)
    DbgPrint("get_module_base:", tostring(name))
    local ok, h = pcall(function() return C.GetModuleHandleA(name) end)
    if not ok then
        DbgPrint("GetModuleHandleA failed for", name, h)
        return nil
    end
    DbgPrint("GetModuleHandleA returned", tostring(h))
    return h
end

local function create_interface(mod, name)
    DbgPrint("create_interface request:", mod, name)
    local h = get_module_base(mod)
    if h == nil then DbgPrint("create_interface: module base nil for", mod) return nil end
    local proc = C.GetProcAddress(h, "CreateInterface")
    local CI = ffi.typeof("void*(__cdecl*)(const char*, int*)")

    -- Fallback: pattern scan for CreateInterface if import not found
    if proc == nil then
        DbgPrint("create_interface: GetProcAddress returned nil, trying pattern scan")
        local pat = "4C 8B 0D ?? ?? ?? ?? 4C 8B D2 4C 8B D9"
        local addr = mem.FindPattern(mod, pat)
        if addr ~= nil then
            proc = addr
            DbgPrint("create_interface: pattern scan found proc", tostring(proc))
        else
            DbgPrint("create_interface: pattern scan failed for", mod)
            return nil
        end
    end

    DbgPrint("create_interface: proc addr", tostring(proc))
    local ok, iface = pcall(function() return ffi.cast(CI, proc)(name, nil) end)
    if not ok then DbgPrint("create_interface: call failed", iface) return nil end
    DbgPrint("create_interface: iface", tostring(iface))
    return iface ~= nil and iface or nil
end

---@format disable-next
local vtable_bind, vtable_thunk = (function()local a=(function()local b=ffi.typeof"void***"return function(c,d,e)return ffi.cast(e,ffi.cast(b,c)[0][d])end end)()local function f(c,d,e,...)local g=a(c,d,ffi.typeof(e,...))return function(...) return g(c,...) end end;local function h(d,e,...) e=ffi.typeof(e,...) return function(c,...) return a(c,d,e)(c,...) end end; return f,h end)()

-- Fixed vtable_hook implementation
local vtable_hook = (function()
    ffi.cdef[[
        int VirtualProtect(void*, uint64_t, unsigned long, unsigned long*);
    ]]
    local unhooks = {}
    local function on_unload()
        for i = #unhooks, 1, -1 do
            xpcall(unhooks[i], print)
            unhooks[i] = nil
        end
    end
    callbacks.Register("Unload", on_unload)

    local ptr_size = ffi.sizeof("void*")

    return function(obj, index, replacement, cdecl, ...)
        local fn_t = ffi.typeof(cdecl, ...)
        local rep = ffi.cast(fn_t, replacement)
        local vtbl = ffi.cast("uintptr_t**", obj)[0]
        local original = vtbl[index]
        local old = ffi.new("unsigned long[1]")
        ffi.C.VirtualProtect(vtbl + index, ptr_size, 0x4, old)
        vtbl[index] = ffi.cast("uintptr_t", rep)
        ffi.C.VirtualProtect(vtbl + index, ptr_size, old[0], old)

        table.insert(unhooks, function()
            ffi.C.VirtualProtect(vtbl + index, ptr_size, 0x4, old)
            vtbl[index] = original
            ffi.C.VirtualProtect(vtbl + index, ptr_size, old[0], old)
        end)

        local orig_fn = ffi.cast(fn_t, original)
        return function(this, ...)
            return orig_fn(this, ...)
        end
    end
end)()

-- Event system with debug
local fire_event, set_event_callback, unset_event_callback
do
    local events = {}
    function fire_event(name, ...)
        DbgPrint("fire_event:", name, "callbacks:", # (events[name] or {}))
        for _, cb in ipairs(events[name] or {}) do
            local ok, err = pcall(cb, ...)
            if not ok then print("[ERROR][fire_event]", name, err, debug.traceback()) end
        end
    end

    function set_event_callback(name, callback)
        DbgPrint("set_event_callback:", name, tostring(callback))
        events[name] = events[name] or {}
        table.insert(events[name], callback)
    end

    function unset_event_callback(name, callback)
        DbgPrint("unset_event_callback:", name, tostring(callback))
        for key, value in ipairs(events[name] or {}) do
            if tostring(value) == tostring(callback) then
                table.remove(events[name], key)
                DbgPrint("unset_event_callback: removed callback at", key)
            end
        end
    end
end

do
    local framestage_t = {
        frame_start = 0,
        frame_net_update_start = 1,
        frame_net_update_postdataupdate_start = 2,
        frame_net_update_postdataupdate_end = 3,
        frame_net_update_end = 4,
        frame_render_start = 5,
        frame_render_end = 6
    }

    local hkFrameStageNotify
    safe_xpcall(function()
        local client_iface = create_interface("client.dll", "Source2Client002")
        if not client_iface then
            DbgPrint("FrameStage: create_interface returned nil for client.dll")
            return
        end
        DbgPrint("FrameStage: got client iface", tostring(client_iface))
        hkFrameStageNotify = vtable_hook(client_iface, 36,
            function(this, stage)
                DbgPrint("hkFrameStageNotify called with stage:", stage)
                for key, value in pairs(framestage_t) do if value == stage then fire_event(key) end end
                return hkFrameStageNotify(this, stage)
            end,
            "void(__thiscall*)(void*, int)"
        )
        DbgPrint("FrameStage: hooked hkFrameStageNotify, hook ptr:", tostring(hkFrameStageNotify))
    end, "hook FrameStage")
end

local function native_GameEntitySystem_GetEntityInstance(self, entindex)
    DbgPrint("native_GameEntitySystem_GetEntityInstance entindex", tostring(entindex))
    if not self then
        DbgPrint("native_GameEntitySystem_GetEntityInstance: self (IGameEntitySystem) is nil")
        return nil
    end
    if not entindex or entindex < 0 then
        DbgPrint("native_GameEntitySystem_GetEntityInstance: invalid entindex", tostring(entindex))
        return nil
    end
    if entindex > 0x7FFE or bit.rshift(entindex, 9) > 0x3F then
        DbgPrint("native_GameEntitySystem_GetEntityInstance: entindex out of bounds", tostring(entindex))
        return nil
    end

    local base = ffi.cast("uintptr_t", self)
    if base == 0 then
        DbgPrint("native_GameEntitySystem: base pointer is 0")
        return nil
    end

    local idxMask = bit.band(entindex, 0x7FFF)
    local pageIndex = bit.rshift(entindex, 9)

    -- Try multiple candidate page offsets and strides to handle layout changes
    local page_offsets = {16, 24, 32}
    local strides = {112, 120, 128, 136}

    for _, page_off in ipairs(page_offsets) do
        local v2 = ffi.cast("uint64_t*", base + 8 * pageIndex + page_off)[0]
        DbgPrint("native_GameEntitySystem v2", tostring(v2), "page_off", tostring(page_off))
        if v2 ~= 0 then
            -- First try the direct slot with multiple strides
            for _, stride in ipairs(strides) do
                local v3 = ffi.cast("uint32_t*", stride * bit.band(entindex, 0x1FF) + v2)
                if v3 ~= nil then
                    local stored = v3[4]
                    local inst = ffi.cast("uint64_t*", v3)[0]
                    if bit.band(stored, 0x7FFF) == idxMask and inst ~= 0 then
                        DbgPrint("native_GameEntitySystem: matched direct slot (stride=", tostring(stride), ")")
                        return inst
                    end
                end
            end
            -- Page scan with multiple strides
            for _, stride in ipairs(strides) do
                for i = 0, 0x1FF do
                    local slot = ffi.cast("uint32_t*", v2 + stride * i)
                    if slot ~= nil then
                        local s = slot[4]
                        local inst = ffi.cast("uint64_t*", slot)[0]
                        if bit.band(s, 0x7FFF) == idxMask and inst ~= 0 then
                            DbgPrint("native_GameEntitySystem: found instance via scan (page_off=", tostring(page_off), ", stride=", tostring(stride), ", i=", tostring(i), ")")
                            return inst
                        end
                    end
                end
            end
        end
    end

    DbgPrint("native_GameEntitySystem: failed to resolve instance for entindex", tostring(entindex))
    return nil
end

local IGameEntitySystem = (function()
    local ok, inst = pcall(function()
        local iface = create_interface("engine2.dll", "GameResourceServiceClientV001")
        if not iface then return nil end
        return ffi.cast("void**", ffi.cast("uintptr_t", iface) + 0x58)[0]
    end)
    if not ok or not inst then
        DbgPrint("IGameEntitySystem: failed to obtain interface", inst)
    else
        DbgPrint("IGameEntitySystem obtained", tostring(inst))
    end
    return inst
end)()

local native_BaseModelEntity_SetModel = (function()
    local ok, found = pcall(function()
        return ffi.cast("void*(__thiscall*)(void*, const char*)", mem.FindPattern("client.dll", "40 53 48 83 EC ?? 48 8B D9 4C 8B C2 48 8B 0D ?? ?? ?? ?? 48 8D 54 24"))
    end)
    if not ok or not found then
        DbgPrint("native_BaseModelEntity_SetModel: pattern not found", found)
        return nil
    end
    DbgPrint("native_BaseModelEntity_SetModel found", tostring(found))
    return found
end)()

local IResourceSystem = create_interface("resourcesystem.dll", "ResourceSystem013")
if IResourceSystem then
    DbgPrint("IResourceSystem obtained", tostring(IResourceSystem))
else
    DbgPrint("IResourceSystem nil")
end
local native_ResourceSystem_PrecacheResource = nil
if IResourceSystem then
    native_ResourceSystem_PrecacheResource = vtable_bind(IResourceSystem, 48, "void*(__thiscall*)(void*, void*, const char*)")
    DbgPrint("bound native_ResourceSystem_PrecacheResource", tostring(native_ResourceSystem_PrecacheResource))
else
    DbgPrint("skip binding PrecacheResource because IResourceSystem nil")
end

-- Robust precache helpers
local function ensure_precache_binding()
    if native_ResourceSystem_PrecacheResource then return true end
    local iface_names = {"ResourceSystem014", "ResourceSystem013", "ResourceSystem012", "ResourceSystem011"}
    for _, iname in ipairs(iface_names) do
        local rs = create_interface("resourcesystem.dll", iname)
        if rs then
            -- try multiple plausible vtable indices
            for _, idx in ipairs({48, 49, 47}) do
                local ok, fn = pcall(function()
                    return vtable_bind(rs, idx, "void*(__thiscall*)(void*, void*, const char*)")
                end)
                if ok and fn then
                    native_ResourceSystem_PrecacheResource = fn
                    IResourceSystem = rs
                    DbgPrint("ensure_precache_binding: bound", iname, "vtbl", tostring(idx))
                    return true
                end
            end
        end
    end
    DbgPrint("ensure_precache_binding: failed to bind ResourceSystem")
    return false
end

local function precache_model_path(path)
    path = normalize_vmdl_path(path)
    if not path or path == "" then return false end
    if not ensure_precache_binding() then return false end
    if not CBufferString then
        DbgPrint("precache_model_path: CBufferString not ready; will retry later")
        return false
    end
    local buf
    local ok_buf, res = pcall(function()
        buf = CBufferString(0, bit.bor(1 * 0x80000000, 0x40000000, 8), nil)
        buf:Insert(0, path, -1, false)
        return true
    end)
    if not ok_buf or not buf then DbgPrint("precache_model_path: failed to build CBufferString", res) return false end

    local type_hints = {"ModelResource", "Model", ""}
    for _, hint in ipairs(type_hints) do
        local ok = pcall(function() return native_ResourceSystem_PrecacheResource(buf, hint) end)
        if ok then
            DbgPrint("precache_model_path: success", path, "type", hint)
            return true
        end
    end
    DbgPrint("precache_model_path: all type hints failed for", path)
    return false
end

CBufferString = (function()
    ffi.cdef [[
        const char* CBufferString_Insert(void*, int, const char*, int, bool) asm("?Insert@CBufferString@@QEAAPEBDHPEBDH_N@Z");
    ]]

    local tier0 = ffi.load "tier0.dll"

    DbgPrint("creating CBufferString metatype")
    return ffi.metatype([[
        struct {
            int m_nLength;
            int m_nAllocatedSize;
            union
            {
                char* m_pString;
                char m_szString[8];
            };
        }
    ]], {
        __index = {
            Insert = tier0.CBufferString_Insert
        }
    })
end)()

-- Prepare buffer
local ok_p, p = pcall(function()
    local val = CBufferString(0, bit.bor(1 * 0x80000000, 0x40000000, 8), nil)
    DbgPrint("CBufferString created", tostring(val))
    return val
end)
if not ok_p or not p then
    DbgPrint("Failed to create CBufferString", p)
else
    local ok_ins, ins_err = pcall(function() p:Insert(0, filename, -1, false) end)
    if not ok_ins then
        DbgPrint("CBufferString:Insert failed", ins_err)
    else
        DbgPrint("CBufferString:Insert succeeded for", filename)
    end
end

local current_model
set_event_callback("frame_render_end", function()
    safe_xpcall(function()
        DbgPrint("frame_render_end callback start")
        local local_pawn = entities.GetLocalPlayer()
        local local_pawn_index = (local_pawn and local_pawn.GetIndex and local_pawn:GetIndex()) or client.GetLocalPlayerIndex()
        DbgPrint("local pawn index:", tostring(local_pawn_index))
        if local_pawn_index == nil then
            current_model = nil
            DbgPrint("no local pawn index; cleared current_model")
            return
        end
        if current_model ~= filename then
            -- Ensure PrecacheResource is bound and called before SetModel
            if not native_ResourceSystem_PrecacheResource then
                local rs = create_interface("resourcesystem.dll", "ResourceSystem013")
                if rs then
                    native_ResourceSystem_PrecacheResource = vtable_bind(rs, 48, "void*(__thiscall*)(void*, void*, const char*)")
                    IResourceSystem = rs
                    DbgPrint("late-bound native_ResourceSystem_PrecacheResource", tostring(native_ResourceSystem_PrecacheResource))
                else
                    DbgPrint("late-bind failed: IResourceSystem still nil")
                end
            end
            if ensure_precache_binding() then
                if precache_model_path(filename) then
                    DbgPrint("PrecacheResource succeeded")
                else
                    DbgPrint("PrecacheResource failed for", filename)
                end
            else
                DbgPrint("cannot precache - missing function or buffer (late)")
            end
            DbgPrint("model change detected - current_model:", tostring(current_model), "desired:", filename)
            if current_model == nil then
                if native_ResourceSystem_PrecacheResource and p then
                    DbgPrint("precaching resource via native_ResourceSystem_PrecacheResource")
                    local ok_precache, prec_err = pcall(function()
                        return native_ResourceSystem_PrecacheResource(p, "ModelResource")
                            or native_ResourceSystem_PrecacheResource(p, "Model")
                            or native_ResourceSystem_PrecacheResource(p, "")
                    end)
                    if not ok_precache then DbgPrint("PrecacheResource failed", prec_err) end
                else
                    DbgPrint("cannot precache - missing function or buffer")
                end
                -- Delay actual SetModel by one frame after precache
                current_model = nil -- force re-attempt on next frame
            end
            DbgPrint("attempting to set model on local pawn instance")
            
            -- Try multiple candidate paths (normalized variants)
            local tried_set = false
            local candidates = build_candidates(filename)
            for _, cand in ipairs(candidates) do
                -- enforce forward slashes and normalized, relative path
                cand = normalize_vmdl_path(cand)
                local buf = make_cbuffer_from_string(CBufferString, cand)
                if buf and native_ResourceSystem_PrecacheResource then
                    pcall(function()
                        return native_ResourceSystem_PrecacheResource(buf, "ModelResource")
                            or native_ResourceSystem_PrecacheResource(buf, "Model")
                            or native_ResourceSystem_PrecacheResource(buf, "")
                    end)
                end
                local local_pawn_instance = native_GameEntitySystem_GetEntityInstance(IGameEntitySystem, local_pawn_index)
                if local_pawn_instance and native_BaseModelEntity_SetModel then
                    local ok_set = pcall(function() native_BaseModelEntity_SetModel(ffi.cast("void*", local_pawn_instance), cand) end)
                    if ok_set then
                        DbgPrint("SetModel succeeded via candidate:", cand)
                        tried_set = true
                        filename = cand
                        break
                    end
                end
            end

            local local_pawn_instance = native_GameEntitySystem_GetEntityInstance(IGameEntitySystem, local_pawn_index)
            if not local_pawn_instance and not tried_set then
                -- Fallback: try to discover the local player via entity list and attempt to set model by iterating plausible indices
                DbgPrint("local_pawn_instance nil; attempting entity-based fallbacks")
                local candidates = {}
                local local_ent = entities.GetLocalPlayer()
                if local_ent and local_ent.GetIndex then
                    table.insert(candidates, local_ent:GetIndex())
                end
                -- Also consider all CCSPlayer entities
                local players = entities.FindByClass and entities.FindByClass("CCSPlayer") or {}
                for i = 1, #players do
                    local ent = players[i]
                    if ent and ent.GetIndex then
                        local idx = ent:GetIndex()
                        if idx then table.insert(candidates, idx) end
                    end
                end
                -- Deduplicate
                local seen = {}
                local uniq = {}
                for _, idx in ipairs(candidates) do if not seen[idx] then seen[idx]=true; table.insert(uniq, idx) end end
                -- Try each candidate index
                for _, idx in ipairs(uniq) do
                    local probe = native_GameEntitySystem_GetEntityInstance(IGameEntitySystem, idx)
                    if probe and native_BaseModelEntity_SetModel then
                        DbgPrint("fallback: attempting set model via candidate idx", tostring(idx), tostring(probe))
                        local ok_set, set_err = pcall(function() native_BaseModelEntity_SetModel(ffi.cast("void*", probe), filename) end)
                        if ok_set then
                            local_pawn_instance = probe
                            DbgPrint("fallback: set model succeeded for idx", tostring(idx))
                            break
                        else
                            DbgPrint("fallback: set model failed for idx", tostring(idx), set_err)
                        end
                    end
                end
                if not local_pawn_instance then
                    DbgPrint("fallback: failed to set model via candidates")
                end
            end
            if local_pawn_instance then
                DbgPrint("calling native_BaseModelEntity_SetModel with instance", tostring(local_pawn_instance), "filename", filename)
                if native_BaseModelEntity_SetModel then
                    local ok_set, set_err = pcall(function() native_BaseModelEntity_SetModel(ffi.cast("void*", local_pawn_instance), filename) end)
                    if not ok_set then DbgPrint("native_BaseModelEntity_SetModel failed", set_err) end
                else
                    DbgPrint("native_BaseModelEntity_SetModel function missing")
                end
            end
        else
            DbgPrint("model already set to filename:", filename)
        end
        local local_ent = entities.GetLocalPlayer()
        current_model = (local_ent and local_ent.GetModelName and local_ent:GetModelName()) or current_model
        DbgPrint("frame_render_end callback end - current_model now:", tostring(current_model))
    end, "frame_render_end")
end)

-- Track seen models and append to a newline-delimited text file
local seen_models = {}
local seen_models_pending = {}
local seen_models_dirty = false
local seen_models_file = "seen_models.txt"

local function load_seen_models_from_file()
    if not file or not file.Read then return end
    local ok, content = pcall(function() return file.Read(seen_models_file) end)
    if not ok or not content then return end
    for line in tostring(content):gmatch("[^\r\n]+") do
        if line and line ~= "" then
            seen_models[line] = true
        end
    end
end

local function add_seen_model(m)
    if not m or m == "" then return end
    if not seen_models[m] then
        seen_models[m] = true
        table.insert(seen_models_pending, m)
        seen_models_dirty = true
        DbgPrint("seen model:", m)
    end
end

-- Load any previous entries on script start
pcall(load_seen_models_from_file)

-- On each frame, collect CCSPlayer model names
set_event_callback("frame_render_end", function()
    local players = entities.FindByClass and entities.FindByClass("CCSPlayer") or {}
    for i = 1, #players do
        local ent = players[i]
        if ent and ent.GetModelName then
            local m = ent:GetModelName()
            if m then add_seen_model(m) end
        end
    end
end)

-- Periodically flush new entries to the text file (every few seconds)
local last_flush = 0
set_event_callback("frame_render_end", function()
    local now = globals.RealTime and globals.RealTime() or 0
    if seen_models_dirty and (now - last_flush) > 5 then
        if file and file.Write and #seen_models_pending > 0 then
            -- Merge pending with existing contents and write once
            local ok_read, existing = pcall(function() return file.Read(seen_models_file) end)
            local new_content = (ok_read and existing) and (existing .. (existing:sub(-1) == "\n" and "" or "\n")) or ""
            for i = 1, #seen_models_pending do
                new_content = new_content .. seen_models_pending[i] .. "\n"
            end
            local ok_write = pcall(function() file.Write(seen_models_file, new_content) end)
            if ok_write then
                DbgPrint("wrote", tostring(#seen_models_pending), "models to", seen_models_file)
            end
        end
        last_flush = now
        seen_models_pending = {}
        seen_models_dirty = false
    end
end)

-- Extra debug: watch for unload
set_event_callback("Unload", function() DbgPrint("Unload event triggered - cleaning up") end)
