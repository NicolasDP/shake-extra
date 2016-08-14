/**
 * @file    app/main.cc
 * @authors Nicolas DI PRIMA <nicolas@di-prima.fr>
 * @date    2016-08-14
 *
 * @Copyright Nicolas DI PRIMA
 */

#include "A.h"
#include "B.h"
#include <iostream>

int main(void)
{
  std::cout << hello()
            << " "
            << world()
            << std::endl;
  return 0;
}
