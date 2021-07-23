%% 写入协议-- udp
-define(DEFAULT_WRITE_PROTOCOL, udp).
%% 批处理大小
-define(DEFAULT_BATCH_SIZE, 32).

%% udp 服务器
-define(DEFAULT_UDP_OPTS, [{host, "127.0.0,1"}, {port, 8089}]).
%% http 服务器
-define(DEFAULT_HTTP_OPTS, [{host, "127.0.0.1"},
  {port, 8086},
  {database, "mydb"},
  {precision, ms},
  {https_enabled, false}]).

%% types
-type precision() :: ns | us | ms | s | m | h.

-type udp_opts() :: [udp_opt()].
-type udp_opt() :: {host, inet:ip_address() | inet:hostname()}| {port, inet:port_number()}.
-type http_opts() :: [http_opt()].

-type http_opt() :: {host, inet:hostname()}
| {port, inet:port_number()}
| {database, string()}
| {username, string()}
| {password, string()}
| {precision, precision()}
| {https_enabled, boolean()}
| {ssl, ssloptions()}.

-type ssloptions() :: [ssloption()].
-type ssloption() :: {versions, [ssl:protocol_version()]}
| {keyfile, file:filename()}
| {certfile, file:filename()}
| {cacertfile, file:filename()}
| {ciphers, string()}.