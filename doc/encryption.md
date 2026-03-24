# Apis: Encryption Transforms

## Overview

Apis provides two cryptographic transforms for the transport
pipeline: encryption (AES-256-CTR) and signing (HMAC-SHA256).
Both are created as transform structs and composed via the
standard transform pipeline. This document covers their usage,
composition, and security properties.


## Creating Transforms

### Encryption

```lisp
(defvar *encryption-key* (ironclad:random-data 32))  ; 32 bytes for AES-256

(defvar *encrypt-xf* (make-encryption-transform *encryption-key*))
```

The key must be an octet vector of the correct length for the
cipher (32 bytes for AES-256). Each call to the apply-fn
generates a fresh random 16-byte IV and prepends it to the
ciphertext. The reverse-fn extracts the IV and decrypts.

Optional keyword arguments `:cipher` and `:mode` default to
`:aes` and `:ctr`. These should not normally be changed.

### Signing

```lisp
(defvar *signing-key* (ironclad:random-data 32))

(defvar *sign-xf* (make-signing-transform *signing-key*))
```

The apply-fn computes HMAC(key, bytes) and appends the 32-byte
digest. The reverse-fn splits off the trailing digest,
recomputes, and verifies. Mismatch signals
`signature-verification-failed` (a subclass of `transport-error`).

Optional keyword argument `:digest` defaults to `:sha256`.


## Composition: Always Sign Then Encrypt

**The recommended and expected composition order is: signing
transform first, encryption transform second.**

```lisp
;; CORRECT: sign first, encrypt second
(list sign-xf encrypt-xf)

;; WRONG: encrypt first, sign second — do not do this
(list encrypt-xf sign-xf)
```

In the transform list, apply-fns execute left to right and
reverse-fns execute right to left:

```
Send:    plaintext → sign → encrypt → ciphertext
Receive: ciphertext → decrypt → verify → plaintext
```

This means:

- The signature covers the original plaintext.
- The signature itself is protected by encryption (not visible
  to an observer).
- On the receive side, decryption happens first, then the HMAC
  is verified against the recovered plaintext.


## Why Encryption Alone Is Not Sufficient

**AES-256-CTR provides confidentiality only, not integrity.**

CTR mode is a stream cipher mode. It XORs the plaintext with a
keystream derived from the key and IV. This has an important
consequence: decryption with the wrong key, or decryption of
tampered ciphertext, does not produce an error. It silently
produces garbage.

This means:

- **Wrong key:** If the receiver has a different key than the
  sender, decryption will complete without error but produce
  meaningless bytes. The Apis deserializer will then fail with
  a read error on the corrupted payload — but the failure message
  will be confusing and far from the root cause.

- **Tampered ciphertext:** An attacker who modifies bytes in the
  ciphertext will cause the corresponding plaintext bytes to
  change. In CTR mode, flipping bit N of the ciphertext flips
  bit N of the plaintext. No error is raised.

- **Truncated ciphertext:** If bytes are removed from the
  ciphertext, decryption produces a shorter plaintext. Again,
  no error from the cipher itself.

**The signing transform provides the integrity check.** When
sign-then-encrypt is used, the HMAC covers the original
plaintext. After decryption, the HMAC is recomputed and compared.
Any of the above failures — wrong key, tampering, truncation —
will cause the HMAC to mismatch, producing a clear
`signature-verification-failed` error that names the actual
problem.


## Using Encryption Without Signing

If you use `make-encryption-transform` alone (without a signing
transform), be aware:

1. You have **no tamper detection**. Modified ciphertext will
   decrypt to modified plaintext without any error.

2. You have **no wrong-key detection**. Decryption with the wrong
   key produces garbage, but the error will surface later as a
   deserialization failure, not as a clear cryptographic error.

3. You have **no authentication**. You cannot verify that the
   message came from the expected sender.

For any use case where integrity or authenticity matters — which
is nearly all of them — use the sign-then-encrypt composition.

Signing without encryption is a valid and useful configuration:
it provides integrity and authenticity while leaving the payload
readable by infrastructure (relays, loggers). This is appropriate
when confidentiality is not required but tamper detection is.


## Per-Operation Policies

The transforms slot on a transport accepts either a flat list
(same transforms for every message) or a function
`(operation-keyword → transform-list)` for per-operation policies:

```lisp
(make-instance 'loopback-transport
  :transforms (lambda (operation)
                (case operation
                  (:payment (list sign-xf encrypt-xf))  ; sign + encrypt
                  (:health-check nil)                     ; no transforms
                  (otherwise (list sign-xf))))            ; sign only
  :local-authority "myhost:9100")
```

The function receives the message's operation keyword and returns
the transform list to apply for that message. On the receive side,
the operation is extracted from the cleartext envelope (which is
never transformed) so the correct reverse transforms are applied.


## Key Management

Keys are captured in closures when the transform is created.
There is no key-store or key slot on the transport. This is the
simplest model: pre-shared keys, established out of band.

### Key rotation

Replace the transport's transforms value with new transforms
built from new keys. Messages in flight at the moment of
rotation will use whichever key was active when they were sent.
The receiver must be able to handle both the old and new key
during the transition window — this is the application's
responsibility.

A future key-negotiation protocol could automate this, but it
is out of scope for the current implementation.

### Key generation

Use `ironclad:random-data` for cryptographic key generation:

```lisp
(ironclad:random-data 32)  ; 32 random bytes for AES-256 / HMAC
```

Do not use deterministic or low-entropy keys in production.
The test suite uses deterministic keys (`make-test-key`) for
reproducibility; this is appropriate only for tests.


## Conditions

- **`transport-error`** — signaled when decryption fails due to
  an ironclad error (e.g., malformed ciphertext shorter than the
  IV length). The `:reason` slot contains a description.

- **`signature-verification-failed`** — a subclass of
  `transport-error`, signaled when HMAC verification fails
  (digest mismatch) or when the signed message is too short to
  contain a signature.


## Full-Message Encryption

The current implementation applies transforms to the payload
only. The envelope (routing metadata: id, from, to, operation,
timestamp, time-to-live, cause) remains in cleartext so that
infrastructure can inspect it.

Full-message encryption — hiding even the metadata — is a
planned extension. It would apply a second set of transforms
to the entire framed message (envelope + payload together)
after framing, with the reverse applied before deframing.
This is not yet implemented.
