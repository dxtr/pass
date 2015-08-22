;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:pass)

(include "termios.h")

;; for some non posix features
(define "_XOPEN_SOURCE")                
(define "_BSD_SOURCE")                  

;;;
(constantenum (cflag :define-constants t)
              #+(or linux bsd)
              ((:cbaud "CBAUD"))
              #+(or linux bsd)
              ((:cbaudex "CBAUDEX"))
              ((:csize "CSIZE"))
              ;; valid character sizes
              ((:cs5 "CS5"))
              ((:cs6 "CS6"))
              ((:cs7 "CS7"))
              ((:cs8 "CS8"))
              ;; valid character sizes
              ((:cstopb "CSTOPB"))
              ((:cread "CREAD"))
              ((:parenb "PARENB"))
              ((:parodd "PARODD"))
              ((:hupcl "HUPCL"))
              ((:clocal "CLOCAL"))
              #-linux
              ((:loblk "LOBLK"))
              #+(or bsd linux)
              ((:cibaud "CIBAUD"))
              #+(or bsd linux)
              ((:cmspar "CMSPAR"))
              #+(or bsd linux)
              ((:crtscts "CRTSCTS")))

;;; lflags
(constantenum (lflag :define-constants t)
              ((:isig "ISIG"))
              ((:icanon "ICANON"))
              #-linux
              ((:xcase "XCASE"))
              ((:echo "ECHO"))
              ((:echoe "ECHOE"))
              ((:echok "ECHOK"))
              ((:echonl "ECHONL"))
              #+(or linux bsd)
              ((:echoctl "ECHOCTL"))
              #+(or linux bsd)
              ((:echoprt "ECHOPRT"))
              #+(or linux bsd)
              ((:echoke "ECHOKE"))
              #-linux
              ((:defecho "DEFECHO"))
              #+bsd
              ((:flusho "FLUSHO"))
              ((:noflsh "NOFLSH"))
              ((:tostop "TOSTOP"))
              #+bsd
              ((:pendin "PENDIN"))
              ((:iexten "IEXTEN")))

;;; iflags
(constantenum (iflag :define-constants t)
              ((:ignbrk "IGNBRK"))
              ((:brkint "BRKINT"))
              ((:ignpar "IGNPAR"))
              ((:parmrk "PARMRK"))
              ((:inpck "INPCK"))
              ((:istrip "ISTRIP"))
              ((:inlcr "INLCR"))
              ((:igncr "IGNCR"))
              ((:icrnl "ICRNL"))
              #+linux
              ((:iuclc "IUCLC"))
              ((:ixon "IXON"))
              ;; XSI features are #+xfi marked in sb-posix grovel file,
              ;; but (find :xsi *features*) return NIL
              ;; so i'm leaving xsi features unmarked 
              ((:ixany "IXANY"))
              ((:ixoff "IXOFF"))
              #-linux
              ((:imaxbel "IMAXBEL"))
              #+linux
              ((:iutf8 "IUTF8")))

;;; oflags
(constantenum (oflag :define-constants t)
              ((:opost "OPOST"))
              #+linux
              ((:olcuc "OLCUC"))
              ((:onlcr "ONLCR"))
              ((:ocrnl "OCRNL"))
              ((:onocr "ONOCR"))
              ((:onlret "ONLRET"))
              ((:ofill "OFILL"))
              #-linux
              ((:ofdel "OFDEL"))
              #+(or linux bsd)
              ((:nldly "NLDLY"))
              #+(or linux bsd)
              ((:crdly "CRDLY"))
              #+(or linux bsd)
              ((:tabdly "TABDLY"))
              #+(or linux bsd)
              ((:bsdly "BSDLY"))
              #+(or linux bsd)
              ((:vtdly "VTDLY"))
              #+(or linux bsd)
              ((:ffdly "FFDLY")))

;;; control characters
(constantenum (control-character :define-constants t)
              ((:vintr "VINTR"))
              ((:vquit "VQUIT"))
              ((:verase "VERASE"))
              ((:vkill "VKILL"))
              ((:veof "VEOF"))
              ((:vmin "VMIN"))
              ((:veol "VEOL"))
              ((:vtime "VTIME"))
              #+linux
              ((:veol2 "VEOL2"))
              #-linux
              ((:vswtch "VSWTCH"))
              ((:vstart "VSTART"))
              ((:vstop "VSTOP"))
              ((:vsusp "VSUSP"))
              #-linux
              ((:vdsusp "VDSUSP"))
              #+linux
              ((:vlnext "VLNEXT"))
              #+linux
              ((:vwerase "VWERASE"))
              #+linux
              ((:vreprint "VREPRINT"))
              #-linux
              ((:vdiscard "VDISCARD"))
              #-linux
              ((:vstatus "VSTATUS")))

(constant (TCSANOW 0))

(ctype tcflag "tcflag_t")

(ctype cc "cc_t")

(ctype termios-speed "speed_t")

(constant (nccs "NCCS"))

(cstruct termios "struct termios" 
	 (iflag         "c_iflag" :type tcflag)
	 (oflag         "c_oflag" :type tcflag)
	 (cflag         "c_cflag" :type tcflag)
	 (lflag         "c_lflag" :type tcflag)
	 (control-chars "c_cc"    :type cc :count nccs))

(defsyscall (%tcgetattr "tcgetattr") (:int :restart t :handle fd)
  (fd      :int)
  (termios :pointer)) 			; const struct termios *

(defsyscall (%tcsetattr "tcsetattr") (:int :restart t :handle fd)
  (fd             :int)
  (optional-ation tcssetattr-action)
  (termios        :pointer))

;;; tcssetattr acti

