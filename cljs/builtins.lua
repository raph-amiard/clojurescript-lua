--   Copyright (c) Rich Hickey. All rights reserved.
--   The use and distribution terms for this software are covered by the
--   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
--   which can be found in the file epl-v10.html at the root of this distribution.
--   By using this software in any fashion, you are agreeing to be bound by
--   the terms of this license.
--   You must not remove this notice, or any other, from this software.

require("bit")
require("json")

builtins = {}
basic_types_prot_functions = {}
js = {}
builtins.functions_metatable = nil

function builtins.create_proto_table()
   local ptable = {}
   setmetatable(ptable, {__index=cljs.core.default_proto_table()})
   return ptable
end

function builtins.getnilproto()
   return (nil).proto_methods
end

function builtins.getbooleanproto()
   return (false).proto_methods
end

function builtins.getfunctionproto()
   return (function()end).proto_methods
end

function builtins.getstringproto()
   return ("").proto_methods
end

function builtins.getnumberproto()
   return (0).proto_methods
end

function newmt() 
   return {__index={proto_methods=builtins.create_proto_table(), __call = builtins.IFnCall}}
end

-- Metatables initialisation
function builtins.init_meta_tables()
   debug.setmetatable(0, newmt())
   debug.setmetatable(false, newmt())
   debug.setmetatable(nil, newmt())
   builtins.functions_metatable = newmt()
   debug.setmetatable(function()end, builtins.functions_metatable)
   getmetatable("").__index.proto_methods=builtins.create_proto_table()
   getmetatable("").__call = builtins.IFnCall
end

function builtins.create_object(...)
   local a = builtins.new_object()
   for i=1,select("#", ...), 2 do
      a[select(i, ...)] = select(i+1, ...)
   end
   return a
end

function builtins.new_object(...)
   local t = {...}
   setmetatable(t, newmt())
   return t
end

function builtins.create_func_object()
   local o = {}
   setmetatable(o, builtins.functions_metatable)
   return o
end

tern_box_val = nil
function box_tern(val)
   tern_box_val = val
   return true
end

function unbox_tern(val)
   return tern_box_val
end

function string:split(sep)
   local sep, fields = sep or ":", {}
   local pattern = string.format("([^%s]+)", sep)
   self:gsub(pattern, function(c) fields[#fields+1] = c end)
   return fields
end

function builtins.create_namespace(str)
   local ns_tables = str:split(".")
   local current_table = _G
   for i=1,#ns_tables do
      if not current_table[ns_tables[i]] then
	 current_table[ns_tables[i]] = {}
      end
      current_table = current_table[ns_tables[i]]
   end
end

function builtins.array_copy(t)
   local new_arr = builtins.array_init({}, 0)
   for i=1,t.length do
      builtins.array_insert(new_arr, t[i])
   end
   return new_arr
end

function builtins.array_remove(arr, pos)
   table.remove(arr, pos)
   arr.length = arr.length - 1
end

function builtins.array(...)
   local t = {...}
   return builtins.array_init(t, select("#", ...))
end

function builtins.array_init(arr, len)
   arr.proto_methods = cljs.core.Array.proto_methods
   arr.constructor = cljs.core.Array
   arr.length = len
   setmetatable(arr, {__call=builtins.IFnCall})
   return arr
end

function builtins.array_len(arr)
   return arr.length
end

function builtins.array_get(arr, idx)
   return arr[idx+1]
end

function builtins.array_set(arr, idx, val)
   arr[idx+1]=val
   arr.length = math.max(arr.length, idx)
end

function builtins.array_insert(arr, val)
   arr[arr.length+1]=val
   arr.length = arr.length + 1
end

function builtins.type(x)
   local t = type(x)
   if t == "table" then
      return x.constructor or "table"
   else
      return t
   end
end

function builtins.keys (obj) 
   local keys = builtins.array()
   for k,v in pairs(obj) do builtins.array_insert(keys, k) end
   return keys
end

function builtins.getUid(x)
end

string.HASHCODE_MAX_ = 0x100000000;

-- Hashcode function borrowed from google closure library
function string.hashCode(str)
   local result = 0
   for i=1,#str do
    result = 31 * result + str:byte(i);
    -- Normalize to 4 byte range, 0 ... 2^32.
    result = result % string.HASHCODE_MAX_;
   end
   return result
end

js.Error = {}
function js.Error.new(msg)
   local inst = {}
   inst.message = msg
   inst.constructor = js.Error
   return inst
end

function builtins.tocomp(a)
   if type(a) == "number" or type(a) == "string" then
      return a
   elseif a == true then
      return 1
   elseif a == false then
      return 0
   end
   return 0
end

function builtins.compare(a, b)
   a = builtins.tocomp(a)
   b = builtins.tocomp(b)
   if a > b then
      return 1 
   elseif a < b then
      return -1
   else 
      return 0
   end
end

function builtins.shuffle(arr, opt_randFn)
  local randFn = opt_randFn or math.random
  for i=#arr+1,1,-1 do
    local j = math.floor(randFn() * (i + 1));
    local tmp = arr[i];
    arr[i] = arr[j];
    arr[j] = tmp;
  end
end

function builtins.sort(t, comp)
   local fncomp = nil
   if comp then
      fncomp = function(x, y) return comp(x, y) < 0 end
   end
   return table.sort(t, fncomp)
end

function builtins.array_to_string(a)
   local b = {}
   for i=0,a.length do
      if a[i] == nil then
	 b[i] = "nil"
      else
	 b[i] = cljs.core.str(a[i])
      end
   end
   return "<Array " .. table.concat(b, ", ") .. ">"
end


function builtins.array_slice (values,i1,i2)
   local res = builtins.array()
   local n = values.length
   -- default values for range
   i1 = i1 or 1
   i2 = i2 or n
   if i2 < 0 then
      i2 = n + i2 + 1
   elseif i2 > n then
      i2 = n
   end
   if i1 < 1 or i1 > n then
      return {}
   end
   for i = i1,i2 do
      builtins.array_insert(res,values[i])
   end
   return res
end

function builtins.get_func(func_obj)
   return getmetatable(func_obj).__call
end

function builtins.IFnCall(obj, ...)
   local len = select("#", ...) + 1
   local fn_name = "cljs__core__IFn___invoke__arity__" .. tostring(len)
   return obj.proto_methods[fn_name](obj, ...)
end

function builtins.mfn_call(self, ...)
   local args = cljs.core.array_seq(builtins.array(...),0);
   return self.proto_methods.cljs__core__IMultiFn___dispatch__arity__2(self,args)
end

builtins.unpack = function(t)
   return builtins.__unpack[t.length](t)
end

builtins.__unpack = {}
builtins.__unpack[0] = function (t)
end
builtins.__unpack[1] = function (t)
   return t[1]
end
builtins.__unpack[2] = function (t)
   return t[1], t[2]
end
builtins.__unpack[3] = function (t)
   return t[1], t[2], t[3]
end
builtins.__unpack[4] = function (t)
   return t[1], t[2], t[3], t[4]
end
builtins.__unpack[5] = function (t)
   return t[1], t[2], t[3], t[4], t[5]
end
builtins.__unpack[6] = function (t)
   return t[1], t[2], t[3], t[4], t[5], t[6]
end
builtins.__unpack[7] = function (t)
   return t[1], t[2], t[3], t[4], t[5], t[6], t[7]
end
builtins.__unpack[8] = function (t)
   return t[1], t[2], t[3], t[4], t[5], t[6], t[7], t[8]
end
builtins.__unpack[9] = function (t)
   return t[1], t[2], t[3], t[4], t[5], t[6], t[7], t[9]
end
builtins.__unpack[10] = function (t)
   return t[1], t[2], t[3], t[4], t[5], t[6], t[7], t[9], t[10]
end
builtins.__unpack[11] = function (t)
   return t[1], t[2], t[3], t[4], t[5], t[6], t[7], t[9], t[10], t[11]
end
builtins.__unpack[12] = function (t)
   return t[1], t[2], t[3], t[4], t[5], t[6], t[7], t[9], t[10], t[11], t[12]
end
builtins.__unpack[13] = function (t)
   return t[1], t[2], t[3], t[4], t[5], t[6], t[7], t[9], t[10], t[11], t[12],t[13]
end
builtins.__unpack[14] = function (t)
   return t[1], t[2], t[3], t[4], t[5], t[6], t[7], t[9], t[10], t[11], t[12],t[13], t[14]
end
builtins.__unpack[15] = function (t)
   return t[1], t[2], t[3], t[4], t[5], t[6], t[7], t[9], t[10], t[11], t[12],t[13], t[15]
end
builtins.__unpack[16] = function (t)
   return t[1], t[2], t[3], t[4], t[5], t[6], t[7], t[9], t[10], t[11], t[12],t[13], t[15], t[16]
end
builtins.__unpack[17] = function (t)
   return t[1], t[2], t[3], t[4], t[5], t[6], t[7], t[9], t[10], t[11], t[12],t[13], t[15], t[16], t[17]
end
builtins.__unpack[18] = function (t)
   return t[1], t[2], t[3], t[4], t[5], t[6], t[7], t[9], t[10], t[11], t[12],t[13], t[15], t[16], t[17], t[18]
end
builtins.__unpack[19] = function (t)
   return t[1], t[2], t[3], t[4], t[5], t[6], t[7], t[9], t[10], t[11], t[12],t[13], t[15], t[16], t[17], t[18], t[19]
end
builtins.__unpack[20] = function (t)
   return t[1], t[2], t[3], t[4], t[5], t[6], t[7], t[9], t[10], t[11], t[12],t[13], t[15], t[16], t[17], t[18], t[19], t[20]
end
builtins.__unpack[21] = function (t)
   return t[1], t[2], t[3], t[4], t[5], t[6], t[7], t[9], t[10], t[11], t[12],t[13], t[15], t[16], t[17], t[18], t[19], t[20], t[21]
end

builtins.type_instance_mt = {__call = builtins.IFnCall }