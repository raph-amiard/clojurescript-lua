builtins = {}
basic_types_prot_functions = {}

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