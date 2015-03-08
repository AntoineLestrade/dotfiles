; http://vim.wikia.com/wiki/Map_caps_lock_to_escape_in_Windows

;CapsOn = false
;CapsLock::
;    Suspend on
;    Send, {ESC}
;    Suspend off
;    return
;
;Esc::
;    if CapsOn = false
;    {
;        CapsOn = true
;        SetCapsLockState, on
;    }
;    else
;    {
;        CapsOn = false
;        SetCapsLockState, off
;    }
;    return

;## Rebind Caps-Lock to CTRL (use AltGr-CapsLock to caps lock)

Esc::Ctrl
<^>!CapsLock::CapsLock
CapsLock::Esc

; For emacs
;CapsLock::Ctrl
;<^>!CapsLock::CapsLock
