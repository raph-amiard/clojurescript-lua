builtins = {}
basic_types_prot_functions = {}
js = {}

require("bit")

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
   local t2 = {}
   for k,v in pairs(t) do
      t2[k] = v
   end
   return t2
end

function builtins.array(...)
   return {...}
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
   local keys = {}
   for k,v in pairs(obj) do table.insert(k) end
   return keys
end

function builtins.getUid(x)
end

string.HASHCODE_MAX_ = 0x100000000;

-- Hashcode function borrowed from google closure library
function string.hashCode(str)
   local result = 0
   for i=0,#str do
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
   return inst
end