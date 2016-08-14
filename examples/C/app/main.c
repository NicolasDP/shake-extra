/**
 * @file    app/main.c
 * @authors Nicolas DI PRIMA <nicolas@di-prima.fr>
 * @date    2016-08-12
 *
 * @Copyright Nicolas DI PRIMA
 */

#include "stdio.h"
#include "pthread.h"

#include "A.h"
#include "B.h"

void* print_it(void* p)
{
  char const* w = (char const*) p;
  printf("%s %s!\n", hello(), w);
  return NULL;
}

int main(void)
{
  pthread_t thread;
  pthread_create(&thread, NULL, print_it, (void*)world());
  pthread_join(thread, NULL);
  return 0;
}
