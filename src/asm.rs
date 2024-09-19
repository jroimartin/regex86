//! Constants required to compile a regular expression into assembly.

/// Prelude of the generated assembly code.
pub(crate) const PRELUDE: &str = r#"
    bits 64

    sys_write  equ 1
    sys_exit   equ 60
    stdout     equ 1

    section .text
    global _start
    ;; Entrypoint.
    ;; Arguments:
    ;; - [rsp]:      argc
    ;; - [rsp+0x8]:  argv[0]
    ;; - [rsp+0x16]: argv[1]
    ;; - ...
_start:
    mov r12, [rsp]       ; r12: argc

    cmp r12, 2
    jne .error_arg

    mov rdi, [rsp+16]
    call match
    cmp rax, 0
    jne .error_match

    mov r12b, [rdi]
    cmp r12b, 0
    jne .error_match

    .success:
    mov rdi, 0
    call exit

    .error_match:
    mov rdi, 1
    call exit

    .error_arg:
    mov rdi, 2
    call exit

    ;; Matches regular expression against a null-terminated string.
    ;; Arguments:
    ;; - rdi: null-terminated string.
    ;; Return value:
    ;; - rax: 0 on success or 1 otherwise.
match:
    call e0
    ret

    ;; Exits process with a exit code.
    ;; Arguments:
    ;; - rdi: exit code.
exit:
    mov rax, sys_exit
    syscall
"#;
