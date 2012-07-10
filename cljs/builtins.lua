builtins = {}
basic_types_prot_functions = {}
js = {}

debug.setmetatable(0, {})

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

function builtins.compare(a, b)
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
    local j = Math.floor(randFn() * (i + 1));
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
   for k,v in pairs(a) do
      b[k] = cljs.core.toString(v)
   end
   return "<Array " .. table.concat(b, ", ") .. ">"
end


builtins.StringBuffer = {}
function builtins.StringBuffer.new()
   sb = {}
   function sb.append (self, str)
      table.insert(str)
   end
   function sb.toString(self, str)
   end
end

function table.slice (values,i1,i2)
   local res = {}
   local n = #values
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
   local k = 1
   for i = i1,i2 do
      res[k] = values[i]
      k = k + 1
   end
   return res
end

function builtins.IFnCall(obj, ...)
   local len = select("#", ...) + 1
   local fn_name = "cljs__core__IFn___invoke__arity__" .. tostring(len)
   return obj.proto_methods[fn_name](obj, ...)
end