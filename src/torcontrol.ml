(* Base for this implementation is control-spec.txt Version 1 *)

open Util
open Quotation

(* Basic types *)

type fingerprint = Fingerprint of Cstruct.t
type name_status = IsNamed | IsNotNamed
type nick_name = Nickname of string
type long_name = Longname of fingerprint * (name_status * nick_name) option

let string_of_fingerprint (Fingerprint f) = "$" ^ hexdig_quote (Cstruct.to_string f)

let name_status_indicator = function
  | IsNamed -> "="
  | IsNotNamed -> "~"

let string_of_long_name (Longname (fp, nick)) =
  match nick with
    | Some (st, (Nickname name)) -> string_of_fingerprint fp
                                      ^ name_status_indicator st ^ name
    | None                       -> string_of_fingerprint fp

type server_spec = SP_Longname of long_name
                 | SP_Nickname of nick_name

let string_of_server_spec = function
  | SP_Longname ln -> string_of_long_name ln
  | SP_Nickname (Nickname nn) -> nn

type hs_address = HSAddress of string

let string_of_hs_address (HSAddress s) = s

(* Possible Purposes *)

type purpose = General | Controller | Bridge

let string_of_purpose = function
  | General -> "general"
  | Controller -> "controller"
  | Bridge -> "bridge"

(* signals that can be sent to tor *)
type signal = Reload
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

let string_of_signal = function
  | Reload -> "RELOAD"
  | Shutdown -> "SHUTDOWN"
  | Dump -> "DUMP"
  | Debug -> "DEBUG"
  | Halt -> "HALT"
  | Hup -> "HUP"
  | Int -> "INT"
  | Usr1 -> "USR1"
  | Usr2 -> "USR2"
  | Term -> "TERM"
  | Newnym -> "NEWNYM"
  | Cleardnscache -> "CLEARDNSCACHE"
  | Heartbeat -> "HEARTBEAT"

(* Representation of descriptors (see rend-spec.txt Section 1.3)
 * This library will only support v2 descriptors.
 *)

(* For now this is fine, but it remains TODO *)

type descriptor = Descriptor of string
let string_of_descriptor (Descriptor s) = s

(* reason for a stream to be closed (see tor-spec.txt sectin 6.3) *)

type reason = Misc
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

let int_of_reason = function
  | Misc -> 1
  | Resolve_failed -> 2
  | Connection_refused -> 3
  | Exit_policy -> 4
  | Resource_limit -> 5
  | Done -> 6
  | Timeout -> 7
  | No_route -> 8
  | Hibernating -> 9
  | Internal -> 10
  | Destroyed -> 11
  | Connection_reset -> 12
  | Tor_protocol -> 13
  | Not_directory -> 14

(* Flags that can be used with the CLOSECIRCUIT command *)

type close_circuit_flag = IfUnused

let string_of_close_circuit_flag = function
  | IfUnused -> "IfUnused"

(* Types related to creation and deletion of onion services *)

type new_key_type = Best | RSA1024
type key = New of new_key_type  | Serialized_RSA1024 of string

let string_of_new_key_type = function
  | Best -> "BEST"
  | RSA1024 -> "RSA1024"

let string_of_key = function
  | New t -> "NEW:" ^ string_of_new_key_type t
  | Serialized_RSA1024 s -> "RSA1024:" ^ s

type add_onion_flag = DiscardPK | Detach | BasicAuth | NonAnonymous

let string_of_add_onion_flag = function
  | DiscardPK -> "DiscardPK"
  | Detach -> "Detach"
  | BasicAuth -> "BasicAuth"
  | NonAnonymous -> "NonAnonymous"

let string_of_port_mapping (a, b) =
  "Port=" ^ string_of_int a ^ "," ^ string_of_int b

type client_auth = ClientAuth of string * string

let string_of_client_auth (ClientAuth (name, blob)) = "ClientAuth=" ^ name ^ ":" ^ blob

(* options for the RESOLVE command *)

type resolve_option = ModeReverse

let string_of_resolve_option = function
  ModeReverse -> "mode=reverse"

(* Event types *)

type circuit_severity = Major | Minor

let circuit_severity_suffix = function
  | Major -> ""
  | Minor -> "_MINOR"

type log_severity = Debug | Info | Notice | Warn | Error

let string_of_log_severity = function
  | Debug -> "DEBUG"
  | Info -> "INFO"
  | Notice -> "NOTICE"
  | Warn -> "WARN"
  | Error -> "ERR"

type status_type = General | Client | Server

let string_of_status_type = function
  | General -> "GENERAL"
  | Client -> "CLIENT"
  | Server -> "SERVER"

type event_type = Circuit of circuit_severity (* circuit status events *)
                | Stream                      (* stream events *)
                | OR_connection               (* onion router connection events *)
                | Bandwidth                   (* bandwidth usage info *)
                | Log of log_severity         (* log msgs *)
                | New_descriptors             (* new descriptors available *)
                | Address_map                 (* new address mapping *)
                | Descriptor_upload           (* someone updloaded a descriptor (we are a dirserver) *)
                | Descriptor_change           (* our descriptor changed *)
                | Status of status_type       (* state updates of the tor daemon *)
                | Guard                       (* guard nodes have changed *)
                | Network_status              (* network status has changed *)
                | Clients_seen                (* seen clients per country *)
                | New_consensus               (* new consensus network status has arrived *)
                | New_buildtime
                | Signal                      (* signal received *)
                | New_conf                    (* the configuration has changed *)
                | Transport_launched
                | Stream_bandwidth
                | Connection_bandwidth
                | Circuit_bandwidth
                | Cell_stats
                | Token_bucket
                | HS_descriptors
                | HS_descriptor_content
                | Network_liveness

let string_of_event_type = function
  | Circuit s             -> "CIRC" ^ circuit_severity_suffix s
  | Stream                -> "STREAM"
  | OR_connection         -> "ORCONN"
  | Bandwidth             -> "BW"
  | Log sev               -> string_of_log_severity sev
  | New_descriptors       -> "NEWDESC"
  | Address_map           -> "ADDRMAP"
  | Descriptor_upload     -> "AUTHDIR_NEWDESCS"
  | Descriptor_change     -> "DESCCHANGED"
  | Status t              -> "STATUS_" ^ string_of_status_type t
  | Guard                 -> "GUARD"
  | Network_status        -> "NS"
  | Clients_seen          -> "CLIENTS_SEEN"
  | New_consensus         -> "NEWCONSENSUS"
  | New_buildtime         -> "BUILDTIMEOUT_SET"
  | Signal                -> "SIGNAL"
  | New_conf              -> "CONF_CHANGED"
  | Transport_launched    -> "TRANSPORT_LAUNCHED"
  | Stream_bandwidth      -> "STREAM_BW"
  | Connection_bandwidth  -> "CONN_BW"
  | Circuit_bandwidth     -> "CIRC_BW"
  | Cell_stats            -> "CELL_STATS"
  | Token_bucket          -> "TB_EMPTY"
  | HS_descriptors        -> "HS_DESC"
  | HS_descriptor_content -> "HS_DESC_CONTENT"
  | Network_liveness      -> "NETWORK_LIVENESS"

let all_events = [ Circuit Major; Circuit Minor; Stream; OR_connection;
                   Bandwidth; Log Debug; Log Info; Log Notice; Log Warn;
                   Log Error; New_descriptors; Address_map; Descriptor_upload;
                   Descriptor_change; Status General; Status Client; Status Server;
                   Guard; Network_status; Clients_seen; New_consensus; New_buildtime;
                   Signal; New_conf; Transport_launched; Connection_bandwidth;
                   Stream_bandwidth; Circuit_bandwidth; Cell_stats; Token_bucket;
                   HS_descriptors; HS_descriptor_content; Network_liveness; ]

(* authentification-related types *)

(* Note: The cookie authentification method will be
 * depreacated in the future and is “almost never safe to use”,
 * so we don't support it *)
type auth_data = Data of Cstruct.t

let string_of_auth_data (Data cs) = hexdig_quote (Cstruct.to_string cs)

(* Notes:
 * - Closestream is specified to have options, but they are
 *   omitted here, because they are unused
 * - SETROUTERPURPOSE is not included, since it has been obsoleted
 *)
type command = Setconf of (string * string option) list
             | Resetconf of (string * string option) list
             | Getconf of string list
             | Setevents of bool * event_type list
             | Authenticate of auth_data option
             | Saveconf
             | Signal of signal
             | Mapaddress of (string * string) list
             | Getinfo of string list
             | Extendcircuit of int * (string list) * purpose option
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
             | Add_onion of key * add_onion_flag list * (int * int) list * client_auth list
             | Del_onion of string
             | Hspost of long_name option * descriptor

(* utility functions *)
let single_line_command key args =
  key  ^ List.fold_left (fun a s -> a ^ " " ^ s) "" args ^ "\r\n"

let multi_line_command key args data =
  "+" ^ key ^  List.fold_left (fun a s -> a ^ " " ^ s) "" args ^ "\r\n"
  ^ data ^ "\r\n.\r\n"

(* conversion to string *)

let string_of_command = function
  | Setconf defs ->
      single_line_command "SETCONF" (List.map (string_of_opt_def quote_string) defs)
  | Resetconf defs ->
      single_line_command "RESETCONF" (List.map (string_of_opt_def id) defs)
  | Getconf defs ->
      single_line_command "GETCONF" defs
  | Setevents (ext, evs) ->
      single_line_command "SETEVENTS" (List.append
        (if ext then [ "EXTENDED"; ] else [])
        (List.map string_of_event_type evs))
  | Authenticate auth_data ->
      single_line_command "AUTHENTICATE"
        (List.map string_of_auth_data (list_of_option auth_data))
  | Saveconf ->
      single_line_command "SAVECONF" []
  | Signal signal ->
      single_line_command "SIGNAL" [ string_of_signal signal; ]
  | Mapaddress addr_list ->
      single_line_command "MAPADDRESS" (List.map (fun (oa, na) -> oa ^ "=" ^ na) addr_list)
  | Getinfo keys ->
      single_line_command "GETINFO" keys
  | Extendcircuit (circuit_id, server_specs, purpose) ->
      single_line_command "EXTENDCIRCUIT"
      (string_of_int circuit_id :: List.append server_specs (List.map string_of_purpose (list_of_option purpose)))
  | Setcircuitpurpose (circuit_id, purpose) ->
      single_line_command "SETCIRCUITPURPOSE"
        [ string_of_int circuit_id; "purpose=" ^ string_of_purpose purpose; ]
  | Attachstream (stream_id, circuit_id, hop_num) ->
      single_line_command "ATTACHSTREAM"
        (List.append [ string_of_int stream_id; string_of_int circuit_id; ]
                     (List.map string_of_int (list_of_option hop_num)))
  | Postdescriptor (purpose, cache, descriptor) ->
      let cache' = List.map (fun c -> "cache=" ^ (if c then "yes" else "no"))
                     (list_of_option cache) in
      let purpose' = List.map (fun p -> "purpose=" ^ string_of_purpose p)
                       (list_of_option purpose) in
      multi_line_command "POSTDESCRIPTOR" (List.append cache' purpose') (string_of_descriptor descriptor)
  | Redirectstream (stream_id, address, port) ->
      single_line_command "REDIRECTSTREAM" (List.append
        [ string_of_int stream_id; address;]
        (List.map string_of_int (list_of_option port)))
  | Closestream (stream_id, reason) ->
      single_line_command "CLOSESTREAM"
        [ string_of_int stream_id; string_of_int (int_of_reason reason); ]
  | Closecircuit (circuit_id, flags) ->
      single_line_command "CLOSECIRCUIT"
        (string_of_int circuit_id :: List.map string_of_close_circuit_flag flags)
  | Quit -> single_line_command "QUIT" []
  | Usefeature features -> single_line_command "USEFEATURE" features
  | Resolve (options, addresses) ->
      single_line_command "RESOLVE" (List.append (List.map string_of_resolve_option options)
                                                 addresses)
  | Protocolinfo versions ->
      single_line_command "PROTOCOLINFO" (List.map string_of_int versions)
  | Loadconf config_text -> multi_line_command "LOADCONF" [] config_text
  | Takeownership -> single_line_command "TAKEOWNERSHIP" []
  | Authchallenge client_nonce ->
      single_line_command "AUTHCHALLENGE" [ "SAFECOOKIE"; hexdig_quote (Cstruct.to_string client_nonce); ]
  | Dropguards -> single_line_command "DROPGUARDS" []
  | Add_onion (key, flags, port_mapping, client_auth) ->
      let flag_string = "Flags=" ^ String.concat "," (List.map string_of_add_onion_flag flags) in
      single_line_command "ADD_ONION"
        (string_of_key key :: flag_string :: (List.append (List.map string_of_port_mapping port_mapping)
                                                          (List.map string_of_client_auth client_auth)))
  | Del_onion service_id -> single_line_command "DEL_ONION" [ service_id; ]
  | Hspost (server, descriptor) ->
      multi_line_command "HSPOST" (List.map string_of_long_name (list_of_option server)) (string_of_descriptor descriptor)
  | Hsfetch (addr, name) ->
      single_line_command "HSFETCH"
        (string_of_hs_address addr :: (List.map string_of_long_name (list_of_option name)))
