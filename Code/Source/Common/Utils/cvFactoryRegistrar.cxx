/* Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * All rights reserved.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Charles Taylor, Nathan Wilson, Ken Wang.
 *
 * See SimVascular Acknowledgements file for additional
 * contributors to the source code. 
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including 
 * without limitation the rights to use, copy, modify, merge, publish, 
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included 
 * in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include "SimVascular.h" 

#include <stdio.h>
#include <string.h>

#include "cvFactoryRegistrar.h"

cvFactoryRegistrar::cvFactoryRegistrar()
{
  for (int i = 0; i < CV_MAX_FACTORY_METHOD_PTRS; i++)
    factoryMethodPtrs[i] = NULL;
}

FactoryMethodPtr cvFactoryRegistrar::GetFactoryMethodPtr( int factory_type )
{
	if (factory_type < 0 || factory_type >= CV_MAX_FACTORY_METHOD_PTRS)
		return 0;
	return factoryMethodPtrs[factory_type];
}

void cvFactoryRegistrar::SetFactoryMethodPtr( int factory_type, FactoryMethodPtr factory_ptr )
{
	if (factory_type >= 0 && factory_type < CV_MAX_FACTORY_METHOD_PTRS)
		factoryMethodPtrs[factory_type] = factory_ptr;
}

void* cvFactoryRegistrar::UseFactoryMethod( int factory_type )
{
	if (factory_type < 0 || factory_type >= CV_MAX_FACTORY_METHOD_PTRS)
		return 0;
	if (factoryMethodPtrs[factory_type] == 0)
		return 0;
	return (*factoryMethodPtrs[factory_type])();
}
