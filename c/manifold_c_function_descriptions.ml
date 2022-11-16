module Descriptions (F : Ctypes.FOREIGN) = struct
  open Ctypes
  open F
  open Manifold_c_types

  (* inbound_group_session_size.h *)

  let inbound_group_session_size =
    foreign "olm_inbound_group_session_size"
      (void @-> returning size_t)

  let inbound_group_session =
    foreign "olm_inbound_group_session"
      (ptr void @-> returning (ptr InboundGroupSession.t))

  let inbound_group_session_last_error =
    foreign "olm_inbound_group_session_last_error"
      (ptr InboundGroupSession.t @-> returning (ptr char))

  let clear_inbound_group_session =
    foreign "olm_clear_inbound_group_session"
      (ptr InboundGroupSession.t (* session *)
       @-> returning size_t)     (* olm_error *)

  let pickle_inbound_group_session_length =
    foreign "olm_pickle_inbound_group_session_length"
      (ptr InboundGroupSession.t (* session *)
       @-> returning size_t)     (* olm_error *)

  let pickle_inbound_group_session =
    foreign "olm_pickle_inbound_group_session"
      (ptr InboundGroupSession.t (* session *)
       @-> ptr void              (* key *)
       @-> size_t                (* key_length *)
       @-> ptr void              (* pickled *)
       @-> size_t                (* pickled_length *)
       @-> returning size_t)     (* olm_error *)

  let unpickle_inbound_group_session =
    foreign "olm_unpickle_inbound_group_session"
      (ptr InboundGroupSession.t (* session *)
       @-> ptr void              (* key *)
       @-> size_t                (* key_length *)
       @-> ptr void              (* pickled *)
       @-> size_t                (* pickled_length *)
       @-> returning size_t)     (* olm_error *)

  let init_inbound_group_session =
    foreign "olm_init_inbound_group_session"
      (ptr InboundGroupSession.t (* session *)
       @-> ptr uint8_t           (* session_key *)
       @-> size_t                (* session_key_length *)
       @-> returning size_t)     (* olm_error *)

  let import_inbound_group_session =
    foreign "olm_import_inbound_group_session"
      (ptr InboundGroupSession.t (* session *)
       @-> ptr uint8_t           (* session_key *)
       @-> size_t                (* session_key_length *)
       @-> returning size_t)     (* olm_error *)

  let group_decrypt_max_plaintext_length =
    foreign "olm_group_decrypt_max_plaintext_length"
      (ptr InboundGroupSession.t (* session *)
       @-> ptr uint8_t           (* message *)
       @-> size_t                (* message_length *)
       @-> returning size_t)     (* olm_error *)

  let group_decrypt =
    foreign "olm_group_decrypt"
      (ptr InboundGroupSession.t (* session *)
       @-> ptr uint8_t           (* message *)
       @-> size_t                (* message_length *)
       @-> ptr uint8_t           (* plaintext *)
       @-> size_t                (* max_plaintext_length *)
       @-> ptr uint32_t          (* message_index *)
       @-> returning size_t)     (* length of decrpyted plain-text or olm_error *)

  let inbound_group_session_id_length =
    foreign "olm_inbound_group_session_id_length"
      (ptr InboundGroupSession.t @-> returning size_t)

  let inbound_group_session_id =
    foreign "olm_inbound_group_session_id"
      (ptr InboundGroupSession.t (* session *)
       @-> ptr uint8_t           (* id *)
       @-> size_t                (* id_length *)
       @-> returning size_t)     (* length of session id or olm_error *)

  let inbound_group_session_first_known_index =
    foreign "olm_inbound_group_session_first_known_index"
      (ptr InboundGroupSession.t @-> returning uint32_t)

  let inbound_group_session_is_verified =
    foreign "olm_inbound_group_session_is_verified"
      (ptr InboundGroupSession.t @-> returning int)

  let export_inbound_group_session_length =
    foreign "olm_export_inbound_group_session_length"
      (ptr InboundGroupSession.t @-> returning size_t)

  let export_inbound_group_session =
    foreign "olm_export_inbound_group_session"
      (ptr InboundGroupSession.t (* session *)
       @-> ptr uint8_t           (* key *)
       @-> size_t                (* key_length *)
       @-> uint32_t              (* message_index *)
       @-> returning size_t)     (* length of ratchet key or olm_error *)

  (* outbound_group_session.h *)

  let outbound_group_session_size =
    foreign "olm_outbound_group_session_size"
      (void @-> returning size_t)

  let outbound_group_session =
    foreign "olm_outbound_group_session"
      (ptr void @-> returning (ptr OutboundGroupSession.t))

  let outbound_group_session_last_error =
    foreign "olm_outbound_group_session_last_error"
      (ptr OutboundGroupSession.t @-> returning (ptr char))

  let clear_outbound_group_session =
    foreign "olm_clear_outbound_group_session"
      (ptr OutboundGroupSession.t (* session *)
       @-> returning size_t)      (* olm_error *)

  let pickle_outbound_group_session_length =
    foreign "olm_pickle_outbound_group_session_length"
      (ptr OutboundGroupSession.t (* session *)
       @-> returning size_t)      (* olm_error *)

  let pickle_outbound_group_session =
    foreign "olm_pickle_outbound_group_session"
      (ptr OutboundGroupSession.t (* session *)
       @-> ptr void               (* key *)
       @-> size_t                 (* key_length *)
       @-> ptr void               (* pickled *)
       @-> size_t                 (* pickled_length *)
       @-> returning size_t)      (* olm_error *)

  let unpickle_outbound_group_session =
    foreign "olm_unpickle_outbound_group_session"
      (ptr OutboundGroupSession.t (* session *)
       @-> ptr void               (* key *)
       @-> size_t                 (* key_length *)
       @-> ptr void               (* pickled *)
       @-> size_t                 (* pickled_length *)
       @-> returning size_t)      (* olm_error *)

  let init_outbound_group_session_random_length =
    foreign "olm_init_outbound_group_session_random_length"
      (ptr OutboundGroupSession.t (* session *)
       @-> returning size_t)      (* olm_error *)

  let init_outbound_group_session =
    foreign "olm_init_outbound_group_session"
      (ptr OutboundGroupSession.t (* session *)
       @-> ptr uint8_t            (* random *)
       @-> size_t                 (* random_length *)
       @-> returning size_t)      (* olm_error *)

  let group_encrypt_message_length =
    foreign "olm_group_encrypt_message_length"
      (ptr OutboundGroupSession.t (* session *)
       @-> size_t                 (* plaintext_length *)
       @-> returning size_t)      (* # bytes that will be created by encryption *)

  let group_encrypt =
    foreign "olm_group_encrypt"
      (ptr OutboundGroupSession.t (* session *)
       @-> ptr uint8_t            (* plaintext *)
       @-> size_t                 (* plaintext_length *)
       @-> ptr uint8_t            (* message *)
       @-> size_t                 (* message_length *)
       @-> returning size_t)      (* length of encrypted message or olm_error *)

  let outbound_group_session_id_length =
    foreign "olm_outbound_group_session_id_length"
      (ptr OutboundGroupSession.t @-> returning size_t)

  let outbound_group_session_id =
    foreign "olm_outbound_group_session_id"
      (ptr OutboundGroupSession.t (* session *)
       @-> ptr uint8_t            (* id *)
       @-> size_t                 (* id_length *)
       @-> returning size_t)      (* length of session id or olm_error *)

  let outbound_group_session_message_index =
    foreign "olm_outbound_group_session_message_index"
      (ptr OutboundGroupSession.t @-> returning uint32_t)

  let outbound_group_session_key_length =
    foreign "olm_outbound_group_session_key_length"
      (ptr OutboundGroupSession.t @-> returning size_t)

  let outbound_group_session_key =
    foreign "olm_outbound_group_session_key"
      (ptr OutboundGroupSession.t (* session *)
       @-> ptr uint8_t            (* key to be used for next message *)
       @-> size_t                 (* key_length *)
       @-> returning size_t)      (* length of ratchet key or olm_error *)

  (* olm.h *)

  let get_library_version =
    foreign "olm_get_library_version"
      (ptr uint8_t         (* major *)
       @-> ptr uint8_t     (* minor *)
       @-> ptr uint8_t     (* patch *)
       @-> returning void)

  let account_size =
    foreign "olm_account_size"
      (void @-> returning size_t)

  let session_size =
    foreign "olm_session_size"
      (void @-> returning size_t)

  let utility_size =
    foreign "olm_utility_size"
      (void @-> returning size_t)

  let account =
    foreign "olm_account"
      (ptr void @-> returning (ptr Account.t))

  let session =
    foreign "olm_session"
      (ptr void @-> returning (ptr Session.t))

  let utility =
    foreign "olm_utility"
      (ptr void @-> returning (ptr Utility.t))

  let error =
    foreign "olm_error"
      (void @-> returning size_t)

  let account_last_error =
    foreign "olm_account_last_error"
      (ptr Account.t @-> returning (ptr char))

  let session_last_error =
    foreign "olm_session_last_error"
      (ptr Session.t @-> returning (ptr char))

  let utility_last_error =
    foreign "olm_utility_last_error"
      (ptr Utility.t @-> returning (ptr char))

  let clear_account =
    foreign "olm_clear_account"
      (ptr Account.t @-> returning size_t)

  let clear_session =
    foreign "olm_clear_session"
      (ptr Session.t @-> returning size_t)

  let clear_utility =
    foreign "olm_clear_utility"
      (ptr Utility.t @-> returning size_t)

  let pickle_account_length =
    foreign "olm_pickle_account_length"
      (ptr Account.t @-> returning size_t)

  let pickle_session_length =
    foreign "olm_pickle_session_length"
      (ptr Session.t @-> returning size_t)

  let pickle_account =
    foreign "olm_pickle_account"
      (ptr Account.t         (* account *)
       @-> ptr void          (* key *)
       @-> size_t            (* key_length *)
       @-> ptr void          (* pickled *)
       @-> size_t            (* pickled_length *)
       @-> returning size_t) (* length of pickled account or olm_error *)

  let pickle_session =
    foreign "olm_pickle_session"
      (ptr Session.t         (* session *)
       @-> ptr void          (* key *)
       @-> size_t            (* key_length *)
       @-> ptr void          (* pickled *)
       @-> size_t            (* pickled_length *)
       @-> returning size_t) (* length of pickled session or olm_error *)

  let unpickle_account =
    foreign "olm_unpickle_account"
      (ptr Account.t         (* account *)
       @-> ptr void          (* key *)
       @-> size_t            (* key_length *)
       @-> ptr void          (* pickled *)
       @-> size_t            (* pickled_length *)
       @-> returning size_t) (* olm_error on failure *)

  let unpickle_session =
    foreign "olm_unpickle_session"
      (ptr Session.t         (* session *)
       @-> ptr void          (* key *)
       @-> size_t            (* key_length *)
       @-> ptr void          (* pickled *)
       @-> size_t            (* pickled_length *)
       @-> returning size_t) (* olm_error on failure *)

  let create_account_random_length =
    foreign "olm_create_account_random_length"
      (ptr Account.t @-> returning size_t)

  let create_account =
    foreign "olm_create_account"
      (ptr Account.t         (* account *)
       @-> ptr void          (* random *)
       @-> size_t            (* random_length *)
       @-> returning size_t) (* olm_error on failure *)

  let account_identity_keys_length =
    foreign "olm_account_identity_keys_length"
      (ptr Account.t @-> returning size_t)

  let account_identity_keys =
    foreign "olm_account_identity_keys"
      (ptr Account.t         (* account *)
       @-> ptr void          (* identity_keys *)
       @-> size_t            (* identity_keys_length *)
       @-> returning size_t) (* olm_error on failure *)

  let account_signature_length =
    foreign "olm_account_signature_length"
      (ptr Account.t @-> returning size_t)

  let account_sign =
    foreign "olm_account_sign"
      (ptr Account.t         (* account *)
       @-> ptr void          (* message *)
       @-> size_t            (* message_length *)
       @-> ptr void          (* signature *)
       @-> size_t            (* signature_length *)
       @-> returning size_t) (* olm_error on failure *)

  let account_one_time_keys_length =
    foreign "olm_account_one_time_keys_length"
      (ptr Account.t @-> returning size_t)

  let account_one_time_keys =
    foreign "olm_account_one_time_keys"
      (ptr Account.t         (* account *)
       @-> ptr void          (* one_time_keys *)
       @-> size_t            (* one_time_keys_length *)
       @-> returning size_t) (* olm_error on failure *)

  let account_mark_keys_as_published =
    foreign "olm_account_mark_keys_as_published"
      (ptr Account.t @-> returning size_t)

  let account_max_number_of_one_time_keys =
    foreign "olm_account_max_number_of_one_time_keys"
      (ptr Account.t @-> returning size_t)

  let account_generate_one_time_keys_random_length =
    foreign "olm_account_generate_one_time_keys_random_length"
      (ptr Account.t         (* account *)
       @-> size_t            (* number_of_keys *)
       @-> returning size_t) (* number of random bytes needed *)

  let account_generate_one_time_keys =
    foreign "olm_account_generate_one_time_keys"
      (ptr Account.t         (* account *)
       @-> size_t            (* number_of_keys *)
       @-> ptr void          (* random *)
       @-> size_t            (* random_length *)
       @-> returning size_t) (* olm_errror on failure *)

  let account_generate_fallback_key_random_length =
    foreign "olm_account_generate_fallback_key_random_length"
      (ptr Account.t @-> returning size_t)

  let account_generate_fallback_key =
    foreign "olm_account_generate_fallback_key"
      (ptr Account.t         (* account *)
       @-> ptr void          (* random *)
       @-> size_t            (* random_length *)
       @-> returning size_t) (* olm_errror on failure *)

  let account_fallback_key_length =
    foreign "olm_account_fallback_key_length"
      (ptr Account.t @-> returning size_t)

  let account_fallback_key =
    foreign "olm_account_fallback_key"
      (ptr Account.t         (* account *)
       @-> ptr void          (* fallback_key *)
       @-> size_t            (* fallback_key_size *)
       @-> returning size_t) (* olm_errror on failure *)

  let create_outbound_session_random_length =
    foreign "olm_create_outbound_session_random_length"
      (ptr Session.t @-> returning size_t)

  let create_outbound_session =
    foreign "olm_create_outbound_session"
      (ptr Session.t         (* session *)
       @-> ptr Account.t     (* account *)
       @-> ptr void          (* their_identity_key *)
       @-> size_t            (* their_identity_key_length *)
       @-> ptr void          (* their_one_time_key *)
       @-> size_t            (* their_one_time_key_length *)
       @-> ptr void          (* random *)
       @-> size_t            (* random_length *)
       @-> returning size_t) (* olm_errror on failure *)

  let create_inbound_session =
    foreign "olm_create_inbound_session"
      (ptr Session.t         (* session *)
       @-> ptr Account.t     (* account *)
       @-> ptr void          (* one_time_key_message *)
       @-> size_t            (* message_length *)
       @-> returning size_t) (* olm_errror on failure *)

  let create_inbound_session_from =
    foreign "olm_create_inbound_session_from"
      (ptr Session.t         (* session *)
       @-> ptr Account.t     (* account *)
       @-> ptr void          (* their_identity_key *)
       @-> size_t            (* their_identity_key_length *)
       @-> ptr void          (* one_time_key_message *)
       @-> size_t            (* message_length *)
       @-> returning size_t) (* olm_errror on failure *)

  let session_id_length =
    foreign "olm_session_id_length"
      (ptr Session.t @-> returning size_t)

  let session_id =
    foreign "olm_session_id"
      (ptr Session.t         (* session *)
       @-> ptr void          (* id *)
       @-> size_t            (* id_length *)
       @-> returning size_t) (* olm_errror on failure *)

  let session_has_received_message =
    foreign "olm_session_has_received_message"
      (ptr Session.t @-> returning int)

  let session_describe =
    foreign "olm_session_describe"
      (ptr Session.t       (* session *)
       @-> ptr char        (* buf *)
       @-> size_t          (* buflen *)
       @-> returning void)

  let matches_inbound_session =
    foreign "olm_matches_inbound_session"
      (ptr Session.t         (* session *)
       @-> ptr void          (* one_time_key_message *)
       @-> size_t            (* message_length *)
       @-> returning size_t) (* 0 if no match, olm_errror on failure *)

  let matches_inbound_session_from =
    foreign "olm_matches_inbound_session_from"
      (ptr Session.t         (* session *)
       @-> ptr void          (* their_identity_key *)
       @-> size_t            (* their_identity_key_length *)
       @-> ptr void          (* one_time_key_message *)
       @-> size_t            (* message_length *)
       @-> returning size_t) (* 0 if no match, olm_errror on failure *)

  let remove_one_time_keys =
    foreign "olm_remove_one_time_keys"
      (ptr Account.t         (* account *)
       @-> ptr Session.t     (* session *)
       @-> returning size_t) (* olm_error on failure *)

  let encrypt_message_type =
    foreign "olm_encrypt_message_type"
      (ptr Session.t @-> returning size_t)

  let encrypt_random_length =
    foreign "olm_encrypt_random_length"
      (ptr Session.t @-> returning size_t)

  let encrypt_message_length =
    foreign "olm_encrypt_message_length"
      (ptr Session.t         (* session *)
       @-> size_t            (* plaintext_length *)
       @-> returning size_t) (* # bytes for next msg for given # text bytes *)

  let encrypt =
    foreign "olm_encrypt"
      (ptr Session.t         (* session *)
       @-> ptr void          (* plaintext *)
       @-> size_t            (* plaintext_length *)
       @-> ptr void          (* random *)
       @-> size_t            (* random_length *)
       @-> ptr void          (* message *)
       @-> size_t            (* message_length *)
       @-> returning size_t) (* olm_errror on failure *)

  let decrypt_max_plaintext_length =
    foreign "olm_decrypt_max_plaintext_length"
      (ptr Session.t         (* session *)
       @-> size_t            (* message_type *)
       @-> ptr void          (* message *)
       @-> size_t            (* message_length *)
       @-> returning size_t) (* max number of bytes or olm_errror on failure *)

  let decrypt =
    foreign "olm_decrypt"
      (ptr Session.t         (* session *)
       @-> size_t            (* message_type *)
       @-> ptr void          (* message *)
       @-> size_t            (* message_length *)
       @-> ptr void          (* plaintext *)
       @-> size_t            (* max_plaintext_length *)
       @-> returning size_t) (* length of the plaintext or olm_errror on failure *)

  let sha256_length =
    foreign "olm_sha256_length"
      (ptr Utility.t @-> returning size_t)

  let sha256 =
    foreign "olm_sha256"
      (ptr Utility.t         (* utility *)
       @-> ptr void          (* input *)
       @-> size_t            (* input_length *)
       @-> ptr void          (* output *)
       @-> size_t            (* output_length *)
       @-> returning size_t) (* olm_error on failure (?) *)

  let ed25519_verify =
    foreign "olm_ed25519_verify"
      (ptr Utility.t         (* utility *)
       @-> ptr void          (* key *)
       @-> size_t            (* key_length *)
       @-> ptr void          (* message *)
       @-> size_t            (* message_length *)
       @-> ptr void          (* signature *)
       @-> size_t            (* signature_length *)
       @-> returning size_t) (* olm_error on failure (?) *)

  (* pk.h *)

  let pk_encryption_size =
    foreign "olm_pk_encryption_size"
      (void @-> returning size_t)

  let pk_encryption =
    foreign "olm_pk_encryption"
      (ptr void @-> returning (ptr PkEncryption.t))

  let pk_encryption_last_error =
    foreign "olm_pk_encryption_last_error"
      (ptr PkEncryption.t @-> returning (ptr char))

  let clear_pk_encryption =
    foreign "olm_clear_pk_encryption"
      (ptr PkEncryption.t @-> returning size_t)

  let pk_encryption_set_recipient_key =
    foreign "olm_pk_encryption_set_recipient_key"
      (ptr PkEncryption.t    (* encryption *)
       @-> ptr void          (* public_key *)
       @-> size_t            (* public_key_length*)
       @-> returning size_t) (* olm_error (?) *)

  let pk_ciphertext_length =
    foreign "olm_pk_ciphertext_length"
      (ptr PkEncryption.t    (* encryption *)
       @-> size_t            (* plaintext_length*)
       @-> returning size_t) (* length of ciphertext for given plaintext length *)

  let pk_mac_length =
    foreign "olm_pk_mac_length"
      (ptr PkEncryption.t @-> returning size_t)

  let pk_key_length =
    foreign "olm_pk_key_length"
      (void @-> returning size_t)

  let pk_encrypt_random_length =
    foreign "olm_pk_encrypt_random_length"
      (ptr PkEncryption.t @-> returning size_t)

  let pk_encrypt =
    foreign "olm_pk_encrypt"
      (ptr PkEncryption.t    (* encryption *)
       @-> ptr void          (* plaintext *)
       @-> size_t            (* plaintext_length*)
       @-> ptr void          (* ciphertext *)
       @-> size_t            (* ciphertext_length*)
       @-> ptr void          (* mac *)
       @-> size_t            (* mac_length*)
       @-> ptr void          (* ephemberal_key *)
       @-> size_t            (* ephemberal_key_size*)
       @-> ptr void          (* random *)
       @-> size_t            (* random_length*)
       @-> returning size_t) (* olm_error on failure *)

  let pk_decryption_size =
    foreign "olm_pk_decryption_size"
      (void @-> returning size_t)

  let pk_decryption =
    foreign "olm_pk_decryption"
      (ptr void @-> returning (ptr PkDecryption.t))

  let pk_decryption_last_error =
    foreign "olm_pk_decryption_last_error"
      (ptr PkDecryption.t @-> returning (ptr char))

  let clear_pk_decryption =
    foreign "olm_clear_pk_decryption"
      (ptr PkDecryption.t @-> returning size_t)

  let pk_private_key_length =
    foreign "olm_pk_private_key_length"
      (void @-> returning size_t)

  let pk_key_from_private =
    foreign "olm_pk_key_from_private"
      (ptr PkDecryption.t    (* decryption *)
       @-> ptr void          (* pubkey *)
       @-> size_t            (* pubkey_length*)
       @-> ptr void          (* privkey *)
       @-> size_t            (* privkey_length*)
       @-> returning size_t) (* olm_error on failure *)

  let pickle_pk_decryption_length =
    foreign "olm_pickle_pk_decryption_length"
      (ptr PkDecryption.t @-> returning size_t)

  let pickle_pk_decryption =
    foreign "olm_pickle_pk_decryption"
      (ptr PkDecryption.t    (* decryption *)
       @-> ptr void          (* key *)
       @-> size_t            (* key_length *)
       @-> ptr void          (* pickled *)
       @-> size_t            (* pickled_length*)
       @-> returning size_t) (* olm_error on failure *)

  let unpickle_pk_decryption =
    foreign "olm_unpickle_pk_decryption"
      (ptr PkDecryption.t    (* decryption *)
       @-> ptr void          (* key *)
       @-> size_t            (* key_length *)
       @-> ptr void          (* pickled *)
       @-> size_t            (* pickled_length*)
       @-> ptr void          (* pubkey *)
       @-> size_t            (* pubkey_length*)
       @-> returning size_t) (* olm_error on failure *)

  let pk_max_plaintext_length =
    foreign "olm_pk_max_plaintext_length"
      (ptr PkDecryption.t    (* decryption *)
       @-> size_t            (* ciphertext_length *)
       @-> returning size_t) (* length of plaintext to result from decrypt *)

  let pk_decrypt =
    foreign "olm_pk_decrypt"
      (ptr PkDecryption.t    (* decryption *)
       @-> ptr void          (* ephemberal_key *)
       @-> size_t            (* ephemberal_key_length *)
       @-> ptr void          (* mac *)
       @-> size_t            (* mac_length*)
       @-> ptr void          (* ciphertext *)
       @-> size_t            (* ciphertext_length*)
       @-> ptr void          (* plaintext *)
       @-> size_t            (* max_plaintext_length*)
       @-> returning size_t) (* olm_error on failure *)

  let pk_get_private_key =
    foreign "olm_pk_get_private_key"
      (ptr PkDecryption.t    (* decryption *)
       @-> ptr void          (* private_key *)
       @-> size_t            (* private_key_length *)
       @-> returning size_t) (* olm_error if given buffer is too small *)

  let pk_signing_size =
    foreign "olm_pk_signing_size"
      (void @-> returning size_t)

  let pk_signing =
    foreign "olm_pk_signing"
      (ptr void @-> returning (ptr PkSigning.t))

  let pk_signing_last_error =
    foreign "olm_pk_signing_last_error"
      (ptr PkSigning.t @-> returning (ptr char))

  let clear_pk_signing =
    foreign "olm_clear_pk_signing"
      (ptr PkSigning.t @-> returning size_t)

  let pk_signing_key_from_seed =
    foreign "olm_pk_signing_key_from_seed"
      (ptr PkSigning.t       (* signing *)
       @-> ptr void          (* pubkey *)
       @-> size_t            (* pubkey_length*)
       @-> ptr void          (* seed *)
       @-> size_t            (* seed_length*)
       @-> returning size_t) (* olm_error on failure *)

  let pk_signing_seed_length =
    foreign "olm_pk_signing_seed_length"
      (void @-> returning size_t)

  let pk_signing_public_key_length =
    foreign "olm_pk_signing_public_key_length"
      (void @-> returning size_t)

  let pk_signature_length =
    foreign "olm_pk_signature_length"
      (void @-> returning size_t)

  let pk_sign =
    foreign "olm_pk_sign"
      (ptr PkSigning.t       (* signing *)
       @-> ptr void          (* message *)
       @-> size_t            (* message_length*)
       @-> ptr void          (* signature *)
       @-> size_t            (* signature_length*)
       @-> returning size_t) (* olm_error on failure *)

  (* sas.h *)

  let sas_last_error =
    foreign "olm_sas_last_error"
      (ptr SAS.t @-> returning (ptr char))

  let sas_size =
    foreign "olm_sas_size"
      (void @-> returning size_t)

  let sas =
    foreign "olm_sas"
      (ptr void @-> returning (ptr SAS.t))

  let clear_sas =
    foreign "olm_clear_sas"
      (ptr SAS.t @-> returning size_t)

  let create_sas_random_length =
    foreign "olm_create_sas_random_length"
      (ptr SAS.t @-> returning size_t)

  let create_sas =
    foreign "olm_create_sas"
      (ptr SAS.t             (* sas *)
       @-> ptr void          (* random *)
       @-> size_t            (* random_length *)
       @-> returning size_t) (* olm_error on failure *)

  let sas_pubkey_length =
    foreign "olm_sas_pubkey_length"
      (ptr SAS.t @-> returning size_t)

  let sas_get_pubkey =
    foreign "olm_sas_get_pubkey"
      (ptr SAS.t             (* sas *)
       @-> ptr void          (* pubkey *)
       @-> size_t            (* pubkey_length *)
       @-> returning size_t) (* olm_error on failure *)

  let sas_set_their_key =
    foreign "olm_sas_set_their_key"
      (ptr SAS.t             (* sas *)
       @-> ptr void          (* their_key *)
       @-> size_t            (* their_key_length *)
       @-> returning size_t) (* olm_error on failure *)

  let sas_is_their_key_set =
    foreign "olm_sas_is_their_key_set"
      (ptr SAS.t @-> returning int)

  let sas_generate_bytes =
    foreign "olm_sas_generate_bytes"
      (ptr SAS.t             (* sas *)
       @-> ptr void          (* info *)
       @-> size_t            (* info_length *)
       @-> ptr void          (* output *)
       @-> size_t            (* output_length *)
       @-> returning size_t) (* olm_error on failure *)

  let sas_mac_length =
    foreign "olm_sas_mac_length"
      (ptr SAS.t @-> returning size_t)

  let sas_calculate_mac =
    foreign "olm_sas_calculate_mac"
      (ptr SAS.t             (* sas *)
       @-> ptr void          (* input *)
       @-> size_t            (* input_length *)
       @-> ptr void          (* info *)
       @-> size_t            (* info_length *)
       @-> ptr void          (* mac *)
       @-> size_t            (* mac_length *)
       @-> returning size_t) (* olm_error on failure *)

  let sas_calculate_mac_long_kdf =
    foreign "olm_sas_calculate_mac_long_kdf"
      (ptr SAS.t             (* sas *)
       @-> ptr void          (* input *)
       @-> size_t            (* input_length *)
       @-> ptr void          (* info *)
       @-> size_t            (* info_length *)
       @-> ptr void          (* mac *)
       @-> size_t            (* mac_length *)
       @-> returning size_t) (* olm_error on failure *)
end
