/**
 * @file    app/main.c
 * @authors Nicolas DI PRIMA <nicolas@di-prima.fr>
 * @date    2016-08-12
 *
 * @Copyright Nicolas DI PRIMA
 */

#include "stdio.h"

#include "A.h"
#include "B.h"

int main(void)
{
  printf("%s %s!\n", hello(), world());
  return 0;
}
