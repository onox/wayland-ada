// SPDX-License-Identifier: Apache-2.0
// Copyright (c) 2020 onox <denkpadje@gmail.com>

#include <errno.h>

int get_errno(void)
{
    // Defined in ISO C99 standard
    return errno;
}
