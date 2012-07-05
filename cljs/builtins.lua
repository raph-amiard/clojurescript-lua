builtins = {}
basic_types_prot_functions = {}

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

function builtins.getUid(x)
end