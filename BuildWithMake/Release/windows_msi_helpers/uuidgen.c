/* Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved. 
 *
 * Portions of the code Copyright (c) 2009-2011 Open Source Medical 
 * Software Corporation, University of California, San Diego.
 * All rights reserved.
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

#define _OLEAUT32_

#include <stdio.h>
#include <unknwn.h>

GUID guid;
WORD* wstrGUID[100];
char strGUID[100];
int count, i;

int main (int argc, char* argv[]) {
  if (argc != 2) {
    fprintf (stderr, "SYNTAX: UUIDGEN <number-of-GUIDs-to-generate>\n");
    return 1;
    }
  count = atoi (argv[1]);
  for (i = 0; i < count; i++) {
    CoCreateGuid (&guid);
    StringFromCLSID (&guid, wstrGUID);
    WideCharToMultiByte (CP_ACP, 0, *wstrGUID, -1, strGUID, MAX_PATH, NULL, NULL);
    printf ("%s\n", strGUID);
    }
  return 0;
}
