#lang something // -*- sth -*-

require
  json
  racket/tcp

def socket->string p:
  def-values local-host local-port remote-host remote-port: tcp-addresses p #t
  format "~a:~a-~a:~a" local-host local-port remote-host remote-port

def session i o:
  let loop
    match read-json i
      == eof: (void)
      blob:
        log-info "Received JSON object: ~v" blob
        write-json blob o; newline o; flush-output o
        (loop)

module+ main
  def port-number: 45678
  def s: tcp-listen port-number 500 #t
  log-info "Listening on port ~a" port-number
  let loop
    def-values i o: tcp-accept s
    log-info "Accepted session ~a" (socket->string i)
    session i o
    log-info "Terminated session ~a" (socket->string i)
    close-input-port i
    close-output-port o
    (loop)
