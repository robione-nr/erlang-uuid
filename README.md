# Erlang - UUID Server
A simple UUID generator for use in applications.

A gen_server is used only when necessary to generate UUID values (Versions 1, 2), otherwise they are generated via function calls (Versions 3 - 5).

I favor simplicity over strict ahderence. This is a common theme in my projects. As a result it is recommended to use this in new projects or a new instance of projects which hold UUIDs in persistent storage. (Esp. for projects using namespace versions.) This project implements all five UUID versions and... generally... adheres to RFC 4122.

##### General Assumptions:
 - One instance running, although the probability for collisions is probably rather smaller for Version 1.
 - This implementation only considers the Leach-Salz / RFC-4122 & DCE 1.1 UUID variant.
 - The output from requests is binary.
 - UUIDs can be converted to/from binary and string (as lists) formats.
 - The gen_server is run in the same EVM as calling modules.

##### Version 1:
 - The clock sequence is always incremented for each UUID requested. This negates determining the resolution of the time source or making conditional checks.
 - The MAC address is used as a node identifier unless one cannot be determined.
 
##### Version 2:
 - As above in V1.
 - Functions only in Linux environments.
 - Since UIDs and GIDs tend to be "smaller" integers, the option is provided to integrate both into the UUID. This is also the default behavior.
 - Accepts lists, binaries and atoms to indicate the user/group. The preferred term is a list.
 
##### Version 4:
 - As per RFC 4122.
 
##### Version 3/5:
 - Erlang defaults to network byte order and is considered as the "native environment." No byte order conversions were done.
 - As per the RFC "The concept of a name and name space should be broadly construed..." Any erlang term can be used as a name or name space. The hash function is passed <<term_to_binary(NameSpace)/binary, term_to_binary(Name)/binary>>.
 - Erlang terms, not UUIDs, are used as name space identifiers. No predetermined values are used (RFC 4122 Appendix C). This is a blantant divergence from the RFC.

### Usage

Starting:
 - start_link/0
 - start_link([])

General use API:

 - nil/0: Returns <<0:128>>
 - new/0: Returns one Version 4 UUID
 - to_string/1: Returns list representation of binary UUID in proper text format.
 - to_binary/1: Returns binary representation of text (as list) formated UUID.
 
Specific Versions:

 - v1/0: Returns one Version 1 UTC/MAC UUID
 - v2/0: Version 2 UUID generated using gid/uid of user that invoked the EVM.
 - v2/1: Specify alternate user. Uses GID/UID hybrid.
 - v2/2: Specify the domain using the constant (UUID_DOMAIN_ +): GROUP, USER, ALL.
 - v3/1: Generate name-based UUID with name space and name passed in a tuple. (Preferred.)
 - v3/2: Name space and Name passed as separate parameters.
 - v4/0: Generate Version 4 random UUID.
 - v5/1: See v3/1.
 - v5/2: See v3/2.
 
### References

[RFC 4122: A Universally Unique IDentifier (UUID) URN Namespace](https://tools.ietf.org/html/rfc4122)

[DCE 1.1: Authentication and Security Services](https://pubs.opengroup.org/onlinepubs/9696989899/chap5.htm#tagcjh_08_02_01_01)
