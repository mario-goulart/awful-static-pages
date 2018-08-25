(cond-expand
  (chicken-4
   (use awful-static-pages))
  (chicken-5
   (import awful-static-pages))
  (else
   (error "Unsupported CHICKEN version.")))

(request-pages
 '("/regex/3"
   "/regex/7"
   "/procedure/foo"
   ))
