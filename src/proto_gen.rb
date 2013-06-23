# -*- coding: utf-8 -*-
# 一些更高级的协议处理，现在用来处理cache_group类型.

# 解析api.txt，将其返回为api的列表.
def parse_api(path)
  file = open(path, "r")
  api = {}
  api_list = []
  file.lines.each do |line|
    line.strip!
    next if line[0,1]=="#"
    array=line.split(":")
    api[array[0]] = array[1]
    if line == ""
      if api["packet_type"] != nil or api["cache_group"] != nil
        api_list << api
      end
      api = {}
    end
  end
  api_list << api
end

def gen_package(file, api, index, length)
  file.write("packet_type:#{api["packet_type"]}\n")
  file.write("name:#{api["name"]}\n")
  file.write("payload:#{api["payload"]}\n")
  file.write("desc:#{api["desc"]}\n")
  file.write("module:#{api["module"]}\n") unless api["module"].nil?
  file.write("\n") if index != length-1
end

# 生成协议组.
def gen_group(file, api, index, length)
  code = api["cache_group"]
  name = api["name"]
  desc = api["desc"]
  api_list = [
              {
                "packet_type" => (code.to_i+1).to_s,
                "name" => name+"_cah",
                "payload" => "db_"+name,
                "desc" => desc
              },
              {
                "packet_type" => (code.to_i+2).to_s,
                "name" => name+"_cah_new",
                "payload" => "db_"+ name.chop(),
                "desc" => desc
              },
              {
                "packet_type" => (code.to_i+3).to_s,
                "name" => name+"_cah_del",
                "payload" => "pt_pkid",
                "desc" => desc
              },
              {
                "packet_type" => (code.to_i+4).to_s,
                "name" => name+"_cah_dels",
                "payload" => "pt_pkids",
                "desc" => desc
              },
              {
                "packet_type" => (code.to_i+5).to_s,
                "name" => name+"_cah_upt",
                "payload" => "db_"+name.chop(),
                "desc" => desc
              }
             ]
  api_list.each_with_index do |a, index2|
    gen_package(file, a, 1, 2)
    if index == length-1 and index2 == api_list.length-1
    else
      file.write("\n")
    end
  end
end

def gen_api(api_list)
  file = open("proto/api.txt", "w")
  api_list.each_with_index do |api, index|
    if api["packet_type"] != nil
      gen_package(file, api, index, api_list.length)
    elsif api["cache_group"] != nil
      gen_group(file, api, index, api_list.length)
    end
  end
  file.close
end

api_list = parse_api("proto/api_2.txt")
gen_api(api_list)
