-define(csv_record(Name), {Name, record_info(fields, Name)}).

%% vip经验配置,默认为0级.
-record(gd_vip_exp, {
          level,                    %% vip升级等级
          exp,                      %% 升级到level需要的经验.
          array
         }).
