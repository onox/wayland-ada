  #include <errno.h>

  /* C_errno -- wrapper functions to errno variable */

  int C_errno() {
      return errno;
  }
  void C_reset_errno() {
      errno = 0;
  }
