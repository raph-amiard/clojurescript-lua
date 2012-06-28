require("posix")
require("io")
require("os")
require("cljs.builtins")

function piped_subprocess(command, ...)
   stdin_rd, stdin_wr = posix.pipe()
   stdout_rd, stdout_wr = posix.pipe()
   stderr_rd, stderr_wr = posix.pipe()
   child = posix.fork()
   if child == 0 then
      stdin_wr:close()
      stdout_rd:close()
      stderr_rd:close()
      posix.dup(stdin_rd, io.stdin)
      posix.dup(stdout_wr, io.stdout)
      posix.dup(stderr_wr, io.stderr)
      posix.execp(command, ...)
   else
      stdin_rd:close()
      stdout_wr:close()
      stderr_wr:close()
      return stdin_wr, stdout_rd, stderr_rd
   end
end

function repl()

   -- Run server
   pin, pout, perr = piped_subprocess("./script/runserver.sh")
   
   -- Wait for the server to be ready
   pout:read()
   local switch_exec = true
   
   while true do
      -- Read one line from standart input
      local resp = pout:read()
      local lua_code = ""
      if resp == ":ready" then
	 local l = nil
	 while true do
	    io.write(" > ")	   
	    l = io.read()
	    if l == "switch-exec" then
	       switch_exec = not switch_exec
	       print("exec : " .. tostring(switch_exec))
	    else break
	    end
	 end
	 pin:write(l .. "\n")
	 pin:flush()
      elseif resp == ":incomplete" then
	 io.write(">>")
	 pin:write(io.read() .. "\n")
	 pin:flush()	 
      elseif resp == ":begin" then
	 resp = pout:read()
	 while resp ~= ":end" do
	    lua_code = lua_code .. "\n" .. resp
	    resp = pout:read()		    
	 end
	 print("------ BEGIN LUA OUTPUT ------")
	 print(lua_code)
	 print("------- END LUA OUTPUT -------")
	 if switch_exec then
	    result = loadstring(lua_code)()
	    if result then print("= " .. tostring(result)) end
	 end
      end
   end
   
end

repl()