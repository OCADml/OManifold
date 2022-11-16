module Descriptions (F : Cstubs.Types.TYPE) = struct
  open Ctypes
  open F

  (* crypto.h constants *)
  let sha256_output_length            = 32
  let curve25519_key_length           = 32
  let curve25519_shared_secret_length = 32
  let curve25519_random_length        = curve25519_key_length
  let ed25519_public_key_length       = 32
  let ed25519_private_key_length      = 64
  let ed25519_random_length           = 32
  let ed25519_signature_length        = 64
  let aes256_key_length               = 32
  let aes256_iv_length                = 16

  (* megolm.h constants *)
  let megolm_ratchet_part_length = 32
  let megolm_ratchet_parts       = 4
  let megolm_ratchet_length      = megolm_ratchet_part_length * megolm_ratchet_parts

  module ErrorCode = struct
    let olm_success                   = constant "OLM_SUCCESS"                   int64_t
    let olm_not_enough_random         = constant "OLM_NOT_ENOUGH_RANDOM"         int64_t
    let olm_output_buffer_too_small   = constant "OLM_OUTPUT_BUFFER_TOO_SMALL"   int64_t
    let olm_bad_message_version       = constant "OLM_BAD_MESSAGE_VERSION"       int64_t
    let olm_bad_message_format        = constant "OLM_BAD_MESSAGE_FORMAT"        int64_t
    let olm_bad_message_mac           = constant "OLM_BAD_MESSAGE_MAC"           int64_t
    let olm_bad_message_key_id        = constant "OLM_BAD_MESSAGE_KEY_ID"        int64_t
    let olm_invalid_base64            = constant "OLM_INVALID_BASE64"            int64_t
    let olm_bad_account_key           = constant "OLM_BAD_ACCOUNT_KEY"           int64_t
    let olm_unknown_pickle_version    = constant "OLM_UNKNOWN_PICKLE_VERSION"    int64_t
    let olm_corrupted_pickle          = constant "OLM_CORRUPTED_PICKLE"          int64_t
    let olm_bad_session_key           = constant "OLM_BAD_SESSION_KEY"           int64_t
    let olm_unknown_message_index     = constant "OLM_UNKNOWN_MESSAGE_INDEX"     int64_t
    let olm_bad_legacy_account_pickle = constant "OLM_BAD_LEGACY_ACCOUNT_PICKLE" int64_t
    let olm_bad_signature             = constant "OLM_BAD_SIGNATURE"             int64_t
    let olm_input_buffer_too_small    = constant "OLM_INPUT_BUFFER_TOO_SMALL"    int64_t
    let olm_sas_their_key_not_set     = constant "OLM_SAS_THEIR_KEY_NOT_SET"     int64_t

    type t =
      | Success
      | NotEnoughRandom
      | OutputBufferTooSmall
      | BadMessageVersion
      | BadMessageFormat
      | BadMessageMac
      | BadMessageKeyId
      | InvalidBase64
      | BadAccountKey
      | UnknownPickleVersion
      | CorruptedPickle
      | BadSessionKey
      | UnknownMessageIndex
      | BadLegacyAccountPickle
      | BadSignature
      | InputBufferTooSmall
      | SasTheirKeyNotSet

    let t = enum "OlmErrorCode"
        [ Success               , olm_success
        ; NotEnoughRandom       , olm_not_enough_random
        ; OutputBufferTooSmall  , olm_output_buffer_too_small
        ; BadMessageVersion     , olm_bad_message_version
        ; BadMessageFormat      , olm_bad_message_format
        ; BadMessageMac         , olm_bad_message_mac
        ; BadMessageKeyId       , olm_bad_message_key_id
        ; InvalidBase64         , olm_invalid_base64
        ; BadAccountKey         , olm_bad_account_key
        ; UnknownPickleVersion  , olm_unknown_pickle_version
        ; CorruptedPickle       , olm_corrupted_pickle
        ; BadSessionKey         , olm_bad_session_key
        ; UnknownMessageIndex   , olm_unknown_message_index
        ; BadLegacyAccountPickle, olm_bad_legacy_account_pickle
        ; BadSignature          , olm_bad_signature
        ; InputBufferTooSmall   , olm_input_buffer_too_small
        ; SasTheirKeyNotSet     , olm_sas_their_key_not_set
        ]
        ~unexpected:(fun _ -> assert false)

    let to_string = function
      | Success                -> "SUCCESS"
      | NotEnoughRandom        -> "NOT_ENOUGH_RANDOM"
      | OutputBufferTooSmall   -> "OUTPUT_BUFFER_TOO_SMALL"
      | BadMessageVersion      -> "BAD_MESSAGE_VERSION"
      | BadMessageFormat       -> "BAD_MESSAGE_FORMAT"
      | BadMessageMac          -> "BAD_MESSAGE_MAC"
      | BadMessageKeyId        -> "BAD_MESSAGE_KEY_ID"
      | InvalidBase64          -> "INVALID_BASE64"
      | BadAccountKey          -> "BAD_ACCOUNT_KEY"
      | UnknownPickleVersion   -> "UNKNOWN_PICKLE_VERSION"
      | CorruptedPickle        -> "CORRUPTED_PICKLE"
      | BadSessionKey          -> "BAD_SESSION_KEY"
      | UnknownMessageIndex    -> "UNKNOWN_MESSAGE_INDEX"
      | BadLegacyAccountPickle -> "BAD_LEGACY_ACCOUNT_PICKLE"
      | BadSignature           -> "BAD_SIGNATURE"
      | InputBufferTooSmall    -> "OLM_INPUT_BUFFER_TOO_SMALL"
      | SasTheirKeyNotSet      -> "OLM_SAS_THEIR_KEY_NOT_SET"
  end

  module Aes256Key = struct
    type t = [ `Aes256Key ] structure
    let t : t typ = structure "_olm_aes256_key"
    let key       = field t "key" (array aes256_key_length uint8_t)
    let ()        = seal t
  end

  module Aes256Iv = struct
    type t = [ `Aes256Iv ] structure
    let t : t typ = structure "_olm_aes256_iv"
    let iv        = field t "iv" (array aes256_iv_length uint8_t)
    let ()        = seal t
  end

  module Curve25519PublicKey = struct
    type t = [ `Curve25519PublicKey ] structure
    let t : t typ  = structure "_olm_curve25519_public_key"
    let public_key = field t "public_key" (array curve25519_key_length uint8_t)
    let ()         = seal t
  end

  module Curve25519PrivateKey = struct
    type t = [ `Curve25519PrivateKey ] structure
    let t : t typ   = structure "_olm_curve25519_private_key"
    let private_key = field t "private_key" (array curve25519_key_length uint8_t)
    let ()          = seal t
  end

  module Curve25519KeyPair = struct
    type t = [ `Curve25519KeyPair ] structure
    let t : t typ   = structure "_olm_curve25519_key_pair"
    let public_key  = field t "public_key" Curve25519PublicKey.t
    let private_key = field t "private_key" Curve25519PrivateKey.t
    let ()          = seal t
  end

  module ED25519PublicKey = struct
    type t = [ `ED25519PublicKey ] structure
    let t : t typ  = structure "_olm_ed25519_public_key"
    let public_key = field t "public_key" (array ed25519_public_key_length uint8_t)
    let ()         = seal t
  end

  module ED25519PrivateKey = struct
    type t = [ `ED25519PrivateKey ] structure
    let t : t typ   = structure "_olm_ed25519_private_key"
    let private_key = field t "private_key" (array ed25519_private_key_length uint8_t)
    let ()          = seal t
  end

  module ED25519KeyPair = struct
    type t = [ `ED25519KeyPair ] structure
    let t : t typ   = structure "_olm_ed25519_key_pair"
    let public_key  = field t "public_key" ED25519PublicKey.t
    let private_key = field t "private_key" ED25519PrivateKey.t
    let ()          = seal t
  end

  module Megolm = struct
    type t = [ `Megolm ] structure
    let t : t typ = structure "Megolm"
    let data      = field t "data" (array megolm_ratchet_parts
                                      (array megolm_ratchet_part_length uint8_t))
    let counter   = field t "counter" uint32_t
    let ()        = seal t
  end

  module InboundGroupSession = struct
    type t = [ `InboundGroupSession ] structure
    let t : t typ            = structure "OlmInboundGroupSession"
    let initial_ratchet      = field t "initial_ratchet" Megolm.t
    let latest_ratchet       = field t "latest_ratchet" Megolm.t
    let signing_key          = field t "signing_key" ED25519PublicKey.t
    let signing_key_verified = field t "signing_key_verified" int
    let last_error           = field t "last_error" ErrorCode.t
    let ()                   = seal t
  end

  module OutboundGroupSession = struct
    type t = [ `OutboundGroupSession ] structure
    let t : t typ   = structure "OlmOutboundGroupSession"
    let ratchet     = field t "ratchet" Megolm.t
    let signing_key = field t "signing_key" ED25519KeyPair.t
    let last_error  = field t "last_error" ErrorCode.t
    let ()          = seal t
  end

  module Account = struct
    type t = [ `Account ] structure
    let t : t typ = structure "OlmAccount"
  end

  module Session = struct
    type t = [ `Session ] structure
    let t : t typ = structure "OlmSession"
  end

  module Utility = struct
    type t = [ `Utility ] structure
    let t : t typ = structure "OlmUtility"
  end

  module PkEncryption = struct
    type t = [ `PkEncryption ] structure
    let t : t typ     = structure "OlmPkEncryption"
    let last_error    = field t "last_error" ErrorCode.t
    let recipient_key = field t "recipient_key" Curve25519PublicKey.t
    let ()            = seal t
  end

  module PkDecryption = struct
    type t = [ `PkDecryption ] structure
    let t : t typ  = structure "OlmPkDecryption"
    let last_error = field t "last_error" ErrorCode.t
    let key_pair   = field t "key_pair" Curve25519KeyPair.t
    let ()         = seal t
  end

  module PkSigning = struct
    type t = [ `PkSigning ] structure
    let t : t typ  = structure "OlmPkSigning"
    let last_error = field t "last_error" ErrorCode.t
    let key_pair   = field t "key_pair" ED25519KeyPair.t
    let ()         = seal t
  end

  module SAS = struct
    type t = [ `SAS ] structure
    let t : t typ      = structure "OlmSAS"
    let last_error     = field t "last_error" ErrorCode.t
    let curve25519_key = field t "curve25519_key" Curve25519KeyPair.t
    let secret         = field t "secret" (array curve25519_shared_secret_length uint8_t)
    let their_key_set  = field t "their_key_set" int
    let ()             = seal t
  end
end
