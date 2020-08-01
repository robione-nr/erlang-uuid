# Erlang - UUID Server
A simple UUID generator for use in applications.

This project implements most UUID versions and... generally... adheres to RFC 4122. In this revision the API has been simplified and the version 2 UUID implementation has been removed. All UUID generation is done through a gen_server. Requests UUIDs can be batched. The last change is the ability to set two options: MAC privacy and UUID version utilized.

I favor simplicity over strict ahderence, a common theme in my projects. Due to how namespace UUIDs are generated, it is recommended to convert any pre-existing UUIDs in persistent storage. 

#### General Behavior / Assumptions:
 - One instance running over complete system; used in one EVM.
 - This implementation only considers the Leach-Salz / RFC-4122 UUID variant.
 - The output is binary, not strings.
 - UUIDs can be converted to/from binary and string (as lists) formats.

#### Version 1:
 - The clock sequence remains constant. No collisions can be produced at current CPU clock speeds.
 - Unless specified during start up a random node ID will be used instead of the MAC address.
  
#### Version 4:
 - As per RFC 4122.
 
#### Version 3/5:
 - Erlang defaults to network byte order and is considered as the "native environment." No byte order conversions are done.
 - As per the RFC "The concept of a name and name space should be broadly construed..." Any erlang term can be used as a name or name space. The hash function is passed <<term_to_binary(NameSpace), term_to_binary(Name)>>.
 - Erlang terms, not UUIDs, are used as name space identifiers. No predetermined values are used (RFC 4122 Appendix C).

### Usage

Starting:
 - start_link/0: Equivalent to [].
 - start_link([Options]): 
 
Options:
 - _(empty)_: Default behavior - UUID V1; hidden MAC.
 - {privacy, true|false}: Uses MAC or random node ID for V1.
 - {version, 1 - 5}: Sets UUID generation method.

API:

 - nil/0: Returns <<0:128>>
 - new/0: Returns one V1 or V4 UUID; gen_server state must indicate v1 or v4.
 - new/1: (Tuple) Returns a single V3 or V5 UUID.
 - new/1: (List) Returns _n_ V3 or V5 UUIDs in a list.
 - new/1: (Integer) Returns _n_ V1 or V4 UUIDs in a list.

 - setopt/1: Set MAC privacy for subsequent UUID generation; sets version for UUID generation.
 - to_string/1: Returns list representation of binary UUID in proper text format.
 - to_binary/1: Returns binary representation of text (as list) formatted UUID.
 - calibrate/0: Indicates to gen_server to recompute time offset used in time-based UUIDs.

 - get_utc/1: Get creation time of V1 UUIDs in nanoseconds.
 - get_utc/2: Specify time interval (second, milli..., micro..., nano...)
  
### References

[RFC 4122: A Universally Unique IDentifier (UUID) URN Namespace](https://tools.ietf.org/html/rfc4122)

[DCE 1.1: Authentication and Security Services](https://pubs.opengroup.org/onlinepubs/9696989899/chap5.htm#tagcjh_08_02_01_01)
