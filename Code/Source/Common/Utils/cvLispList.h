/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved.
 *
 * Portions of the code Copyright (c) 2009-2011 Open Source Medical
 * Software Corporation, University of California, San Diego.
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
 *
 *=========================================================================*/

// Adapted from the Berkeley list package, extracted from the NACHOS
// OS simulation system.  Additions / modifications (as marked).

/*
Copyright (c) 1992-1996 The Regents of the University of California.
All rights reserved.

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose, without fee, and without written agreement is
hereby granted, provided that the above copyright notice and the following
two paragraphs appear in all copies of this software.

IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
*/


// list.h 
//	Data structures to manage LISP-like lists.  
//
//      As in LISP, a list can contain any type of data structure
//	as an item on the list: thread control blocks, 
//	pending interrupts, etc.  Allocation and deallocation of the
//	items on the list are to be done by the caller.
//
// Copyright (c) 1992-1996 The Regents of the University of California.
// All rights reserved.  See copyright.h for copyright notice and limitation 
// of liability and disclaimer of warranty provisions.


#ifndef CVLISPLIST_BERKELEY_LIST_H
#define CVLISPLIST_BERKELEY_LIST_H


#include <assert.h>
#define Assert assert


typedef int Bool;

template <class T> class cvLispListIterator;

// The following class defines a "list element" -- which is
// used to keep track of one item on a list.  It is equivalent to a
// LISP cell, with a "car" ("next") pointing to the next element on the list,
// and a "cdr" ("item") pointing to the item on the list.
//
// This class is private to this module (and classes that inherit
// from this module). Made public for notational convenience.

template <class T>
class cvLispListElement {
  public:
    cvLispListElement(T itm); 	// initialize a list element
    cvLispListElement *next;     	// next element on list, NULL if this is last
    T item; 	   	     	// item on the list
};

// The following class defines a "list" -- a singly linked list of
// list elements, each of which points to a single item on the list.
// The class has been tested only for primitive types (ints, pointers);
// no guarantees it will work in general.  For instance, all types
// to be inserted into a list must have a "==" operator defined.

template <class T>
class cvLispList {
  public:
    cvLispList();			// initialize the list
    virtual ~cvLispList();	// de-allocate the list

    virtual void Prepend(T item);// Put item at the beginning of the list
    virtual void Append(T item); // Put item at the end of the list

    T Front() { return first->item; }
    				// Return first item on list
				// without removing it
    T RemoveFront(); 		// Take item off the front of the list
    void Remove(T item); 	// Remove specific item from list

    Bool IsInList(T item) const;// is the item in the list?

    unsigned int NumInList() { return numInList;};
    				// how many items in the list?
    Bool IsEmpty() { return (numInList == 0); };
    				// is the list empty? 

    void Apply(void (*f)(T)) const; 
    				// apply function to all elements in list

    virtual void SanityCheck() const;	
				// has this list been corrupted?
    void SelfTest(T *p, int numEntries);
				// verify module is working

  protected:
    cvLispListElement<T> *first; 	// Head of the list, NULL if list is empty
    cvLispListElement<T> *last;	// Last element of list
    int numInList;		// number of elements in list

friend class cvLispListIterator<T>;
};

// The following class defines a "sorted list" -- a singly linked list of
// list elements, arranged so that "Remove" always returns the smallest 
// element. 
// All types to be inserted onto a sorted list must have a "Compare"
// function defined:
//	   int Compare(T x, T y) 
//		returns -1 if x < y
//		returns 0 if x == y
//		returns 1 if x > y

template <class T>
class cvSortedList : public cvLispList<T> {
  public:
    cvSortedList(int (*comp)(T x, T y)) : cvLispList<T>() { compare = comp;};
    ~cvSortedList() {};		// base class destructor called automatically

    void Insert(T item); 	// insert an item onto the list in sorted order

    void SanityCheck() const;	// has this list been corrupted?
    void SelfTest(T *p, int numEntries);
				// verify module is working

  private:
    int (*compare)(T x, T y);	// function for sorting list elements

    void Prepend(T item) { Insert(item); }  // *pre*pending has no meaning 
				             //	in a sorted list
    void Append(T item) { Insert(item); }   // neither does *ap*pend 

};

// The following class can be used to step through a list. 
// Example code:
//	cvLispListIterator<T> *iter(list); 
//
//	for (; !iter->IsDone(); iter->Next()) {
//	    Operation on iter->Item()
//      }

template <class T>
class cvLispListIterator {
  public:
    cvLispListIterator(cvLispList<T> *list) { current = list->first; } 
				// initialize an iterator

    Bool IsDone() { return current == NULL; };
				// return TRUE if we are at the end of the list

    T Item() { Assert(!IsDone()); return current->item; };
				// return current element on list

    void Next() { current = current->next; };		
				// update iterator to point to next

  private:
    cvLispListElement<T> *current;  // where we are in the list
};

#include "cvLispList.cxx"       // templates are really like macros
				// so needs to be included in every
				// file that uses the template

#endif // BERKELEY_LIST_H
