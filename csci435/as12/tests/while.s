################################################################################
                                          # C- Compiled to IA-32 Code
                                          # Compiler v. 0.1.0
################################################################################
                                          # Global Variables
                                          # 
################################################################################
################################################################################
.globl main
          .type     main, @function       # "main" is type function
main:                                     # 
          enter     $0, $0                # save stack & frame ptrs
          subl      $4, %esp              # allocate local variable x
          movl      $10, %eax             # integer literal
          pushl     %eax                  # save assigning value
          movl      -4(%ebp), %eax        # local variable
          popl      -4(%ebp)              # pop value into variable
          movl      -4(%ebp), %eax        # pass result
.L0:                                      # 
                                          # relational expression
          movl      -4(%ebp), %eax        # local variable
          pushl     %eax                  # stash left operand
          movl      $0, %eax              # integer literal
          popl      %ebx                  # restore left operand
          cmpl      %eax, %ebx            # comparision
          setg      %al                   # relation
          movzbl    %al,%eax              # return result
          cmpl      $0, %eax              # test condition
          je        .L1                   # 
          movl      -4(%ebp), %eax        # local variable
          pushl     %eax                  # push function argument onto stack
          call      output                # invoke function
          movl      -4(%ebp), %eax        # local variable
          decl      -4(%ebp)              # increment/decrement variable
          movl      -4(%ebp), %eax        # pass result
          addl      $0, %esp              # deallocate local variables
          jmp       .L0                   # 
.L1:                                      # 
          addl      $4, %esp              # deallocate local variables
          leave                           # 
          ret                             # 
################################################################################
                                          # Conversion string
          .section  .rodata               # 
.outStr:  .string   "%d\n"                
################################################################################
                                          # Output Routine
.globl output
          .type     output, @function     # "output" is type function
output:                                   # 
          enter     $0, $0                # save stack & frame ptrs
          pushl     8(%ebp)               # retrieve the integer to output
          pushl     $.outStr              # push conversion spec
          call      printf                # call printf
          leave                           # reset stack & frame pointers
          ret                             # return to caller
