type fingerprint = Fingerprint of Cstruct.t
type name_status = IsNamed | IsNotNamed
type nick_name = Nickname of string
type long_name = Longname of fingerprint * (name_status * nick_name) option
val string_of_fingerprint : fingerprint -> string
val name_status_indicator : name_status -> string
val string_of_long_name : long_name -> string
type server_spec = SP_Longname of long_name | SP_Nickname of nick_name
val string_of_server_spec : server_spec -> string
type hs_address = HSAddress of string
val string_of_hs_address : hs_address -> string
type purpose = General | Controller | Bridge
val string_of_purpose : purpose -> string
type signal =
    Reload
  | Shutdown
  | Dump
  | Debug
  | Halt
  | Hup
  | Int
  | Usr1
  | Usr2
  | Term
  | Newnym
  | Cleardnscache
  | Heartbeat
val string_of_signal : signal -> string
type descriptor = Descriptor of string
val string_of_descriptor : descriptor -> string
type reason =
    Misc
  | Resolve_failed
  | Connection_refused
  | Exit_policy
  | Destroyed
  | Done
  | Timeout
  | No_route
  | Hibernating
  | Internal
  | Resource_limit
  | Connection_reset
  | Tor_protocol
  | Not_directory
val int_of_reason : reason -> int
type close_circuit_flag = IfUnused
val string_of_close_circuit_flag : close_circuit_flag -> string
type new_key_type = Best | RSA1024
type key = New of new_key_type | Serialized_RSA1024 of string
val string_of_new_key_type : new_key_type -> string
val string_of_key : key -> string
type add_onion_flag = DiscardPK | Detach | BasicAuth | NonAnonymous
val string_of_add_onion_flag : add_onion_flag -> string
val string_of_port_mapping : int * int -> string
type client_auth = ClientAuth of string * string
val string_of_client_auth : client_auth -> string
type resolve_option = ModeReverse
val string_of_resolve_option : resolve_option -> string
type circuit_severity = Major | Minor
val circuit_severity_suffix : circuit_severity -> string
type log_severity = Debug | Info | Notice | Warn | Error
val string_of_log_severity : log_severity -> string
type status_type = General | Client | Server
val string_of_status_type : status_type -> string
type event_type =
    Circuit of circuit_severity
  | Stream
  | OR_connection
  | Bandwidth
  | Log of log_severity
  | New_descriptors
  | Address_map
  | Descriptor_upload
  | Descriptor_change
  | Status of status_type
  | Guard
  | Network_status
  | Clients_seen
  | New_consensus
  | New_buildtime
  | Signal
  | New_conf
  | Transport_launched
  | Stream_bandwidth
  | Connection_bandwidth
  | Circuit_bandwidth
  | Cell_stats
  | Token_bucket
  | HS_descriptors
  | HS_descriptor_content
  | Network_liveness
val string_of_event_type : event_type -> string
val all_events : event_type list
type auth_data = Data of Cstruct.t
val string_of_auth_data : auth_data -> string
type command =
    Setconf of (string * string option) list
  | Resetconf of (string * string option) list
  | Getconf of string list
  | Setevents of bool * event_type list
  | Authenticate of auth_data option
  | Saveconf
  | Signal of signal
  | Mapaddress of (string * string) list
  | Getinfo of string list
  | Extendcircuit of int * string list * purpose option
  | Setcircuitpurpose of int * purpose
  | Attachstream of int * int * int option
  | Postdescriptor of purpose option * bool option * descriptor
  | Redirectstream of int * string * int option
  | Closestream of int * reason
  | Closecircuit of int * close_circuit_flag list
  | Quit
  | Usefeature of string list
  | Resolve of resolve_option list * string list
  | Protocolinfo of int list
  | Loadconf of string
  | Takeownership
  | Authchallenge of Cstruct.t
  | Dropguards
  | Hsfetch of hs_address * long_name option
  | Add_onion of key * add_onion_flag list * (int * int) list *
      client_auth list
  | Del_onion of string
  | Hspost of long_name option * descriptor
val single_line_command : string -> string list -> string
val multi_line_command : string -> string list -> string -> string
val string_of_command : command -> string
