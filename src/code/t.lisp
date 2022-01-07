(with-output-to-mark (out (current-point))
  (with-open-file (in "/var/spool/mail/matt")
    (mh::read-message in out)
    (mh::read-message in out)
    (mh::read-message in out)
    (ed::msg "~A" (peek-char () in ()))))

(mh::incorporate-local (car (value mail-drops)))
