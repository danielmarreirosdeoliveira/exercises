show_passwd = with { :ok, file } = File.open("passwd"),
                   content       = IO.read(file, :all),
                   :ok           = File.close(file),
                   [a,uid,gid]   = Regex.run(~r/lisa:(\d+):(\d+)/,content)
              do
		"Content: #{a} #{uid}, #{gid}"
              end

IO.puts show_passwd
