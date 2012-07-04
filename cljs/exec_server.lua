require("json")
require("io")
require("cljs.builtins")

actions = {
   exec = function (body)
      local resp = {}
      local func, error = loadstring(body)
      if func then
	 local result = func()
         io.flush()
	 if result then 
	    resp.body = tostring(result)
	 end
	 resp.status = "OK"
      else
	 resp.status = "ERROR"
         resp.body = error
      end
      return resp
   end
}

function exec_server()
   local pipe_out_name = io.read()
   local pipe_in_name = io.read()
   local pipe_out = io.open(pipe_out_name, "w")
   local pipe_in = io.open(pipe_in_name, "r")

   while true do
      local a = pipe_in:read()
      local request = json.decode(a)
      pipe_out:write(json.encode(actions[request.action](request.body)) .. "\n")
      pipe_out:flush()
   end
end

exec_server()