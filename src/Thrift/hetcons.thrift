/**
 * THIS FILE WILL NOT COMPILE DIRECTLY WITH THRIFT
 * Specifically, thrift const structs do not allow fields to be declared using
 * previously defined const structs. Instead, the programmer must copy and paste
 * the definition of the previous struct into the field of the new one.
 * To fix this, I've written flatten_const.py, which does exactly that.
 * 
 * TO COMPILE THIS FILE
 * run flatten_const.py
 */

/**
 * Thrift files can namespace, package, or prefix their output in various
 * target languages.
 * This does not seem to do anything in Haskell
 */
namespace cpp  hetcons
namespace d    hetcons
namespace dart hetcons
namespace java hetcons
namespace php  hetcons
namespace perl hetcons
namespace haxe hetcons
namespace hs   hetcons


typedef set<i8> Hash_Sha2_Descriptor
const Hash_Sha2_Descriptor SUPPORTED_HASH_SHA2_DESCRIPTOR = [32, 48, 64]
exception No_Supported_Hash_Sha2_Descriptor_Provided {
  1: optional Hash_Sha2_Descriptor offending_hash_sha2_descriptor,
  2: optional Hash_Sha2_Descriptor supported_hash_sha2_descriptor,
  3: optional string               explanation
}
typedef binary Hash_Sha2
exception Descriptor_Does_Not_Match_Hash_Sha2 {
  1: Hash_Sha2_Descriptor hash_sha2_descriptor,
  2: Hash_Sha2            hash_sha2
  3: optional string              explanation
}

typedef set<i8> Hash_Sha3_Descriptor
const Hash_Sha3_Descriptor SUPPORTED_HASH_SHA3_DESCRIPTOR = [64]
exception No_Supported_Hash_Sha3_Descriptor_Provided {
  1: optional Hash_Sha3_Descriptor offending_hash_sha3_descriptor,
  2: optional Hash_Sha3_Descriptor supported_hash_sha3_descriptor,
  3: optional string               explanation
}
typedef binary Hash_Sha3
exception Descriptor_Does_Not_Match_Hash_Sha3 {
  1: Hash_Sha3_Descriptor hash_sha3_descriptor,
  2: Hash_Sha3            hash_sha3
  3: optional string              explanation
}




struct Hash_Type_Descriptor {
  2: optional Hash_Sha2_Descriptor sha2,
  3: optional Hash_Sha3_Descriptor sha3
}
const Hash_Type_Descriptor SUPPORTED_HASH_TYPE_DESCRIPTOR = { 'sha2' : SUPPORTED_HASH_SHA2_DESCRIPTOR }
exception No_Supported_Hash_Type_Descriptor_Provided {
  1: optional Hash_Type_Descriptor offending_hash_type_descriptor,
  2: optional Hash_Type_Descriptor supported_hash_type_descriptor,
  3: optional string               explanation
}

union Hash {
  2: Hash_Sha2 sha2,
  3: Hash_Sha3 sha3
}
exception Descriptor_Does_Not_Match_Hash {
  1: Hash_Type_Descriptor hash_type_descriptor,
  2: Hash                 hash
  3: optional string      explanation
}



///////////////////////////  PUBLIC CRYPTO KEYS  ///////////////////////////////
typedef binary Public_Crypto_Key_X509
exception Invalid_Public_Crypto_Key_X509 {
  1: Public_Crypto_Key_X509 offending_public_crypto_key_x509,
  3: optional string        explanation
}

typedef  binary Public_Crypto_Key_PGP
exception Invalid_Public_Crypto_Key_PGP {
  1: Public_Crypto_Key_PGP offending_public_crypto_key_pgp,
  3: optional string       explanation
}

struct Public_Crypto_Key_Type_Descriptor {
  1: optional bool public_crypto_key_x509,
  2: optional bool public_crypto_key_pgp
}
const Public_Crypto_Key_Type_Descriptor SUPPORTED_PUBLIC_CRYPTO_KEY_TYPE_DESCRIPTOR = {'public_crypto_key_x509':true}
exception No_Supported_Public_Crypto_Key_Type_Descriptor_Provided {
  1: optional Public_Crypto_Key_Type_Descriptor offending_public_crypto_key_type_descriptor,
  2: optional Public_Crypto_Key_Type_Descriptor supported_public_crypto_key_type_descriptor,
  3: optional string                            explanation
}

union Public_Crypto_Key {
  1: Public_Crypto_Key_X509 public_crypto_key_x509,
  2: Public_Crypto_Key_PGP  public_crypto_key_pgp
}
exception Descriptor_Does_Not_Match_Public_Crypto_Key {
  1: Public_Crypto_Key_Type_Descriptor public_crypto_key_type_descriptor,
  2: Public_Crypto_Key                 public_crypto_key,
  3: optional string                   explanation
}


/////////////////////////////////  CRYPTO IDs  /////////////////////////////////
struct Crypto_ID_Hash_Type_Descriptor {
  1: Hash_Type_Descriptor hash
  2: optional Public_Crypto_Key_Type_Descriptor public_crypto_key_type_descriptor
}
const Crypto_ID_Hash_Type_Descriptor SUPPORTED_CRYPTO_ID_HASH_TYPE_DESCRIPTOR = {'hash':SUPPORTED_HASH_TYPE_DESCRIPTOR,
                                                                                  'public_crypto_key_type_descriptor':SUPPORTED_PUBLIC_CRYPTO_KEY_TYPE_DESCRIPTOR}
struct Crypto_ID_Hash {
  1: Hash hash
  2: optional Public_Crypto_Key_Type_Descriptor public_crypto_key_type_descriptor
}
exception Descriptor_Does_Not_Match_Crypto_ID_Hash {
  1: Crypto_ID_Hash_Type_Descriptor crypto_id_hash_type_descriptor,
  2: Crypto_ID_Hash                 crypto_id_hash,
  3: optional string                explanation
}


struct Crypto_ID_Type_Descriptor {
  1: optional Public_Crypto_Key_Type_Descriptor public_crypto_key,
  2: optional Crypto_ID_Hash_Type_Descriptor    crypto_id_hash
}
const Crypto_ID_Type_Descriptor SUPPORTED_CRYPTO_ID_TYPE_DESCRIPTOR = {'public_crypto_key':SUPPORTED_PUBLIC_CRYPTO_KEY_TYPE_DESCRIPTOR}
exception No_Supported_Crypto_ID_Type_Descriptor_Provided {
  1: optional Crypto_ID_Type_Descriptor offending_crypto_id_type_descriptor,
  2: optional Crypto_ID_Type_Descriptor supported_crypto_id_type_descriptor,
  3: optional string                    explanation
}

union Crypto_ID {
  1: Public_Crypto_Key public_crypto_key,
  2: Crypto_ID_Hash    crypto_id_hash
}
exception Descriptor_Does_Not_Match_Crypto_ID {
  1: Crypto_ID_Type_Descriptor crypto_id_type_descriptor,
  2: Crypto_ID                 crypto_id,
  3: optional string           explanation
}



///////////////////////////////  SIGNED HASH  ///////////////////////////////////////
typedef string Wishful_Thinking

struct Signed_Hash_Type_Descriptor {
  2: optional Hash_Type_Descriptor      hash_type_descriptor,
  3: optional Crypto_ID_Type_Descriptor crypto_id
}
const Signed_Hash_Type_Descriptor SUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR = {
 'hash_type_descriptor':SUPPORTED_HASH_TYPE_DESCRIPTOR,
 'crypto_id':SUPPORTED_CRYPTO_ID_TYPE_DESCRIPTOR }


struct Signed_Hash {
  1: binary signature,
  2: optional Hash_Type_Descriptor hash_type_descriptor,
  3: optional Crypto_ID            crypto_id
}
exception Invalid_Signed_Hash {
  1: Signed_Hash                 signed_hash,
  3: optional string             explanation
}
exception Descriptor_Does_Not_Match_Signed_Hash {
  1: Signed_Hash_Type_Descriptor signed_hash_type_descriptor,
  2: Signed_Hash                 signed_hash,
  3: optional string             explanation
}


////////////////////////////////// SIGNED MESSAGE ///////////////////////////////
typedef binary Hashable_Message // Should be another message type serialized with thrift
exception Unparsable_Hashable_Message { 1: Hashable_Message message
  3: optional string explanation
}
struct Signed_Message {
  1: Hashable_Message payload
  2: Signed_Hash signature
}



///////////////////////////////// TIMESTAMP /////////////////////////////
typedef i64 Timestamp // The (signed 64-bit integer) number of NANOSECONDS since Unix Epoch

///////////////////////////////// ADDRESS   ///////////////////////////
struct IPv4_Address {
  1: byte byte_0
  2: byte byte_1
  3: byte byte_2
  4: byte byte_3
}
struct IPv6_Address {
  1:  byte byte_0
  2:  byte byte_1
  3:  byte byte_2
  4:  byte byte_3
  5:  byte byte_4
  6:  byte byte_5
  7:  byte byte_6
  8:  byte byte_7
  9:  byte byte_8
  10: byte byte_9
  11: byte byte_10
  12: byte byte_11
  13: byte byte_12
  14: byte byte_13
  15: byte byte_14
  16: byte byte_15
}

typedef string DNS_Name
union Host_Address {
  1: IPv4_Address ipv4_address
  2: IPv6_Address ipv6_address
  3: DNS_Name     dns_name
}
typedef i32 Port_Number
struct Address {
  1: Host_Address host_address
  2: Port_Number  port_number
}
exception Invalid_Address {
  1: Address offending_address
  3: optional string explanation
}


///////////////////////////////////////// PARTICIPANT ID  //////////////
struct Participant_ID {
  1: Address address
  2: Crypto_ID crypto_id
}

///////////////////////////////////////// Observers ///////////////
typedef set<Participant_ID> Participant_Set
typedef set<Participant_ID> Quorum

struct Observer_Trust_Constraint {
  1: Participant_ID  observer_1
  2: Participant_ID  observer_2
  3: Participant_Set safe
  4: Participant_Set live
}
typedef set<Observer_Trust_Constraint>   Observer_Graph
exception Impossible_Observer_Graph {
  1: Observer_Graph offending_observer_graph
  3: optional string explanation
}


typedef map<Participant_ID, set<Quorum>> Observer_Quorums

union Observers {
  1: Observer_Graph   observer_graph,
  2: Observer_Quorums observer_quorums
}





//////////////////////////////// 1a ///////////////////////////////
typedef binary Value_Payload // the type of whatever we're consenting on
typedef i64 Slot
struct Value {  // Technically, this is what the consensus agrees to. We'll need "conflict_with" functions, as well as "garbage collect" functions for any redefinition of this. Otherwise, it's opaque.
  1: Value_Payload value_payload,
  2: Slot          slot
}

struct Proposal_1a { // remember, this will be wrapped in a Signed_Message
  1: Value     value,
  2: Timestamp timestamp,
  3: optional Observers observers
}
exception Invalid_Proposal_1a {
  1: Proposal_1a offending_proposal,
  3: optional string explanation
}

/////////////////////////////////  2a  ///////////////////////////////

struct Phase_2a {
  1: set<Signed_Message /* Phase_1b */> phase_1bs
}
// This can be said to "contain" a timestampted value, which is deduced deterministically from the 1b messages contained

// For instance, if multiple different values are included in the phase 1b messages
exception Invalid_Phase_2a {
  1: Phase_2a offending_phase_2a
  3: optional string explanation
}



/////////////////////////////////  1b  ///////////////////////////////

struct Phase_1b { // remember, this will be wrapped in a Signed_Message
 1: Signed_Message proposal, // wraps a Proposal_1a
 2: set<Signed_Message /*Phase_1a*/>  conflicting_phase2as // past phase2a s which contain a conflicting value.
}


//////////////////////////////////  2b  ////////////////////////////////////
struct Phase_2b {
  1: set<Signed_Message /* Phase_1b */> phase_1bs
}
// remember, this will be wrapped in a Signed_Message

// For instance, if multiple different values are included in the phase 1b messages
exception Invalid_Phase_2b {
  1: Phase_2b offending_phase_2b
  3: optional string explanation
}

struct Proof_of_Consensus {
  1: set<Signed_Message /* Phase_2b */> phase_2bs
}
// ultimately contains a value, proof of the whole consensus process which consented to that value, and optionally the observer graph defining the necessary quorums

// For instance, if multiple different values are included in the phase 2b messages
exception Invalid_Proof_of_Consensus {
  1: Proof_of_Consensus offending_proof_of_consensus
  3: optional string explanation
}







/**
 * Ahh, now onto the cool part, defining a service. Services just need a name
 * and can optionally inherit from another service using the extends keyword.
 */
service Hetcons_Participant {
   void ping(),
   void proposal_1a (1:Signed_Message/*Proposal_1a*/ proposal) // do we actually want this to return void? Perhaps we should return, like, whether or not consensus was reached? Should we have proposers implement their own service so that they can be informed of stuff?
                    throws (1: No_Supported_Hash_Sha2_Descriptor_Provided              no_supported_hash_sha2_descriptor_provided,
                            2: Descriptor_Does_Not_Match_Hash_Sha2                     descriptor_does_not_match_hash_sha2,
                            3: No_Supported_Hash_Sha3_Descriptor_Provided              no_supported_hash_sha3_descriptor_provided,
                            4: Descriptor_Does_Not_Match_Hash_Sha3                     descriptor_does_not_match_hash_sha3,
                            5: No_Supported_Hash_Type_Descriptor_Provided              no_supported_hash_type_descriptor_provided,
                            6: Descriptor_Does_Not_Match_Hash                          descriptor_does_not_match_hash,
                            7: Invalid_Public_Crypto_Key_X509                          invalid_public_crypto_key_X509,
                            8: Invalid_Public_Crypto_Key_PGP                           invalid_public_crypto_key_PGP,
                            9: No_Supported_Public_Crypto_Key_Type_Descriptor_Provided no_supported_public_crypto_key_type_descriptor_provided,
                            10:Descriptor_Does_Not_Match_Public_Crypto_Key             descriptor_does_not_match_public_crypto_key,
                            11:Descriptor_Does_Not_Match_Crypto_ID_Hash                descriptor_does_not_match_crypto_ID_hash,
                            12:No_Supported_Crypto_ID_Type_Descriptor_Provided         no_supported_crypto_ID_type_descriptor_provided,
                            13:Descriptor_Does_Not_Match_Crypto_ID                     descriptor_does_not_match_crypto_ID,
                            14:Invalid_Signed_Hash                                     invalid_signed_hash,
                            15:Descriptor_Does_Not_Match_Signed_Hash                   descriptor_does_not_match_signed_hash,
                            16:Unparsable_Hashable_Message                             unparsable_hashable_message,
                            17:Invalid_Address                                         invalid_address,
                            18:Impossible_Observer_Graph                               impossible_observer_graph,
                            19:Invalid_Proposal_1a                                     invalid_proposal_1a
                            20:Invalid_Phase_2a                                        invalid_Phase_2a
                            21:Invalid_Phase_2b                                        invalid_Phase_2b
                            22:Invalid_Proof_of_Consensus                              invalid_Proof_of_Consensus)

   void phase_1b (1:Signed_Message/*Phase_1b*/ phase_1b_message)
                    throws (1: No_Supported_Hash_Sha2_Descriptor_Provided              no_supported_hash_sha2_descriptor_provided,
                            2: Descriptor_Does_Not_Match_Hash_Sha2                     descriptor_does_not_match_hash_sha2,
                            3: No_Supported_Hash_Sha3_Descriptor_Provided              no_supported_hash_sha3_descriptor_provided,
                            4: Descriptor_Does_Not_Match_Hash_Sha3                     descriptor_does_not_match_hash_sha3,
                            5: No_Supported_Hash_Type_Descriptor_Provided              no_supported_hash_type_descriptor_provided,
                            6: Descriptor_Does_Not_Match_Hash                          descriptor_does_not_match_hash,
                            7: Invalid_Public_Crypto_Key_X509                          invalid_public_crypto_key_X509,
                            8: Invalid_Public_Crypto_Key_PGP                           invalid_public_crypto_key_PGP,
                            9: No_Supported_Public_Crypto_Key_Type_Descriptor_Provided no_supported_public_crypto_key_type_descriptor_provided,
                            10:Descriptor_Does_Not_Match_Public_Crypto_Key             descriptor_does_not_match_public_crypto_key,
                            11:Descriptor_Does_Not_Match_Crypto_ID_Hash                descriptor_does_not_match_crypto_ID_hash,
                            12:No_Supported_Crypto_ID_Type_Descriptor_Provided         no_supported_crypto_ID_type_descriptor_provided,
                            13:Descriptor_Does_Not_Match_Crypto_ID                     descriptor_does_not_match_crypto_ID,
                            14:Invalid_Signed_Hash                                     invalid_signed_hash,
                            15:Descriptor_Does_Not_Match_Signed_Hash                   descriptor_does_not_match_signed_hash,
                            16:Unparsable_Hashable_Message                             unparsable_hashable_message,
                            17:Invalid_Address                                         invalid_address,
                            18:Impossible_Observer_Graph                               impossible_observer_graph,
                            19:Invalid_Proposal_1a                                     invalid_proposal_1a
                            20:Invalid_Phase_2a                                        invalid_Phase_2a
                            21:Invalid_Phase_2b                                        invalid_Phase_2b
                            22:Invalid_Proof_of_Consensus                              invalid_Proof_of_Consensus)
}

service Hetcons_Observer {
   void ping(),
   void phase_2b (1:Signed_Message/*Phase_2b*/ phase_2b_message)
                    throws (1: No_Supported_Hash_Sha2_Descriptor_Provided              no_supported_hash_sha2_descriptor_provided,
                            2: Descriptor_Does_Not_Match_Hash_Sha2                     descriptor_does_not_match_hash_sha2,
                            3: No_Supported_Hash_Sha3_Descriptor_Provided              no_supported_hash_sha3_descriptor_provided,
                            4: Descriptor_Does_Not_Match_Hash_Sha3                     descriptor_does_not_match_hash_sha3,
                            5: No_Supported_Hash_Type_Descriptor_Provided              no_supported_hash_type_descriptor_provided,
                            6: Descriptor_Does_Not_Match_Hash                          descriptor_does_not_match_hash,
                            7: Invalid_Public_Crypto_Key_X509                          invalid_public_crypto_key_X509,
                            8: Invalid_Public_Crypto_Key_PGP                           invalid_public_crypto_key_PGP,
                            9: No_Supported_Public_Crypto_Key_Type_Descriptor_Provided no_supported_public_crypto_key_type_descriptor_provided,
                            10:Descriptor_Does_Not_Match_Public_Crypto_Key             descriptor_does_not_match_public_crypto_key,
                            11:Descriptor_Does_Not_Match_Crypto_ID_Hash                descriptor_does_not_match_crypto_ID_hash,
                            12:No_Supported_Crypto_ID_Type_Descriptor_Provided         no_supported_crypto_ID_type_descriptor_provided,
                            13:Descriptor_Does_Not_Match_Crypto_ID                     descriptor_does_not_match_crypto_ID,
                            14:Invalid_Signed_Hash                                     invalid_signed_hash,
                            15:Descriptor_Does_Not_Match_Signed_Hash                   descriptor_does_not_match_signed_hash,
                            16:Unparsable_Hashable_Message                             unparsable_hashable_message,
                            17:Invalid_Address                                         invalid_address,
                            18:Impossible_Observer_Graph                               impossible_observer_graph,
                            19:Invalid_Proposal_1a                                     invalid_proposal_1a
                            20:Invalid_Phase_2a                                        invalid_Phase_2a
                            21:Invalid_Phase_2b                                        invalid_Phase_2b
                            22:Invalid_Proof_of_Consensus                              invalid_Proof_of_Consensus)
}
