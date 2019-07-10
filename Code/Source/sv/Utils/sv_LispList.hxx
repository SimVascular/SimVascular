/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
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
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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

#include "SimVascular.h"

#define TRUE  1
#define FALSE 0

#include <assert.h>
#define cvAssert assert

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

    T Item() { cvAssert(!IsDone()); return current->item; };
				// return current element on list

    void Next() { current = current->next; };
				// update iterator to point to next

  private:
    cvLispListElement<T> *current;  // where we are in the list
};

// Adapted from the Berkeley list package, extracted from the NACHOS
// OS simulation system.

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


// list.cc
//     	Routines to manage a singly linked list of "things".
//	Lists are implemented as templates so that we can store
//	anything on the list in a type-safe manner.
//
// 	A "cvLispListElement" is allocated for each item to be put on the
//	list; it is de-allocated when the item is removed. This means
//      we don't need to keep a "next" pointer in every object we
//      want to put on a list.
//
//     	NOTE: Mutual exclusion must be provided by the caller.
//  	If you want a synchronized list, you must use the routines
//	in synchlist.cc.
//
// Copyright (c) 1992-1996 The Regents of the University of California.
// All rights reserved.  See copyright.h for copyright notice and limitation
// of liability and disclaimer of warranty provisions.


//----------------------------------------------------------------------
// cvLispListElement<T>::cvLispListElement
// 	Initialize a list element, so it can be added somewhere on a list.
//
//	"itm" is the thing to be put on the list.
//----------------------------------------------------------------------

template <class T>
cvLispListElement<T>::cvLispListElement(T itm)
{
     item = itm;
     next = NULL;	// always initialize to something!
}


//----------------------------------------------------------------------
// cvLispList<T>::cvLispList
//	Initialize a list, empty to start with.
//	Elements can now be added to the list.
//----------------------------------------------------------------------

template <class T>
cvLispList<T>::cvLispList()
{
    first = last = NULL;
    numInList = 0;
}

//----------------------------------------------------------------------
// cvLispList<T>::~cvLispList
//	Prepare a list for deallocation.
//----------------------------------------------------------------------

template <class T>
cvLispList<T>::~cvLispList()
{
    cvAssert(this->IsEmpty());		// make sure list is empty
}

//----------------------------------------------------------------------
// cvLispList<T>::Append
//      Append an "item" to the end of the list.
//
//	Allocate a cvLispListElement to keep track of the item.
//      If the list is empty, then this will be the only element.
//	Otherwise, put it at the end.
//
//	"item" is the thing to put on the list.
//----------------------------------------------------------------------

template <class T>
void
cvLispList<T>::Append(T item)
{
    cvLispListElement<T> *element = new cvLispListElement<T>(item);

    cvAssert(!this->IsInList(item));
    if (this->IsEmpty()) {		// list is empty
	first = element;
	last = element;
    } else {			// else put it after last
	last->next = element;
	last = element;
    }
    numInList++;
    cvAssert(this->IsInList(item));
}

//----------------------------------------------------------------------
// cvLispList<T>::Prepend
//	Same as Append, only put "item" on the front.
//----------------------------------------------------------------------

template <class T>
void
cvLispList<T>::Prepend(T item)
{
    cvLispListElement<T> *element = new cvLispListElement<T>(item);

    cvAssert(!this->IsInList(item));
    if (this->IsEmpty()) {		// list is empty
	first = element;
	last = element;
    } else {			// else put it before first
	element->next = first;
	first = element;
    }
    numInList++;
    cvAssert(this->IsInList(item));
}

//----------------------------------------------------------------------
// cvLispList<T>::RemoveFront
//      Remove the first "item" from the front of the list.
//	List must not be empty.
//
// Returns:
//	The removed item.
//----------------------------------------------------------------------

template <class T>
T
cvLispList<T>::RemoveFront()
{
    cvLispListElement<T> *element = first;
    T thing;

    cvAssert(!this->IsEmpty());

    thing = first->item;
    if (first == last) {	// list had one item, now has none
        first = NULL;
	last = NULL;
    } else {
        first = element->next;
    }
    numInList--;
    delete element;
    return thing;
}

//----------------------------------------------------------------------
// cvLispList<T>::Remove
//      Remove a specific item from the list.  Must be in the list!
//----------------------------------------------------------------------

template <class T>
void
cvLispList<T>::Remove(T item)
{
    cvLispListElement<T> *prev, *ptr;
    T removed;

    cvAssert(this->IsInList(item));

    // if first item on list is match, then remove from front
    if (item == first->item) {
        removed = RemoveFront();
        cvAssert(item == removed);
    } else {
	prev = first;
        for (ptr = first->next; ptr != NULL; prev = ptr, ptr = ptr->next) {
            if (item == ptr->item) {
		prev->next = ptr->next;
		if (prev->next == NULL) {
		    last = prev;
		}
		delete ptr;
		numInList--;
		break;
	    }
        }
        cvAssert(ptr != NULL);	// should always find item!
    }
    cvAssert(!this->IsInList(item));
}

//----------------------------------------------------------------------
// cvLispList<T>::IsInList
//      Return TRUE if the item is in the list.
//----------------------------------------------------------------------

template <class T>
Bool
cvLispList<T>::IsInList(T item) const
{
    cvLispListElement<T> *ptr;

    //    cout << "List<T>::IsInList(T item) const; ";
    //    cout << "item = " << item << "; ";
    //    cout << "this = " << this << "; ";
    //    cout << "first = " << first << ";" << endl;

    for (ptr = first; ptr != NULL; ptr = ptr->next) {

      //        cout << "\tptr = " << ptr << endl;

        if (item == ptr->item) {
            return TRUE;
        }
    }
    return FALSE;
}


//----------------------------------------------------------------------
// cvLispList<T>::Apply
//      Apply function to every item on a list.
//
//	"func" -- the function to apply
//----------------------------------------------------------------------

template <class T>
void
cvLispList<T>::Apply(void (*func)(T)) const
{
    cvLispListElement<T> *ptr;

    for (ptr = first; ptr != NULL; ptr = ptr->next) {
        (*func)(ptr->item);
    }
}


//----------------------------------------------------------------------
// SortedList::Insert
//      Insert an "item" into a list, so that the list elements are
//	sorted in increasing order.
//
//	Allocate a cvLispListElement to keep track of the item.
//      If the list is empty, then this will be the only element.
//	Otherwise, walk through the list, one element at a time,
//	to find where the new item should be placed.
//
//	"item" is the thing to put on the list.
//----------------------------------------------------------------------

template <class T>
void
cvSortedList<T>::Insert(T item)
{
    cvLispListElement<T> *element = new cvLispListElement<T>(item);
    cvLispListElement<T> *ptr;		// keep track

    cvAssert(!this->IsInList(item));
    if (this->IsEmpty()) {			// if list is empty, put at front
        this->first = element;
        this->last = element;
    } else if (compare(item, (this->first)->item) < 0) {  // item goes at front
	element->next = this->first;
	this->first = element;
    } else {		// look for first elt in list bigger than item
        for (ptr =this->first; ptr->next != NULL; ptr = ptr->next) {
            if (compare(item, ptr->next->item) < 0) {
		element->next = ptr->next;
	        ptr->next = element;
		this->numInList++;
		return;
	    }
	}
	(this->last)->next = element;		// item goes at end of list
	this->last = element;
    }
    this->numInList++;
    cvAssert(this->IsInList(item));
}

//----------------------------------------------------------------------
// cvLispList::SanityCheck
//      Test whether this is still a legal list.
//
//	Tests: do I get to last starting from first?
//	       does the list have the right # of elements?
//----------------------------------------------------------------------

template <class T>
void
cvLispList<T>::SanityCheck() const
{
    cvLispListElement<T> *ptr;
    int numFound;

    if (first == NULL) {
        cvAssert((numInList == 0) && (last == NULL));
    } else if (first == last) {
        cvAssert((numInList == 1) && (last->next == NULL));
    } else {
        for (numFound = 1, ptr = first; ptr != last; ptr = ptr->next) {
	    numFound++;
            cvAssert(numFound <= numInList);	// prevent infinite loop
        }
        cvAssert(numFound == numInList);
        cvAssert(last->next == NULL);
    }
}

//----------------------------------------------------------------------
// cvLispList::SelfTest
//      Test whether this module is working.
//----------------------------------------------------------------------

template <class T>
void
cvLispList<T>::SelfTest(T *p, int numEntries)
{
    int i;
    cvLispListIterator<T> *iterator = new cvLispListIterator<T>(this);

    SanityCheck();
    // check various ways that list is empty
    cvAssert(this->IsEmpty() && (first == NULL));
    for (; !iterator->IsDone(); iterator->Next()) {
      //	cvAssertNOTREACHED();	// nothing on list
    }

    for (i = 0; i < numEntries; i++) {
	 Append(p[i]);
         cvAssert(this->IsInList(p[i]));
         cvAssert(!this->IsEmpty());
     }
     SanityCheck();

     // should be able to get out everything we put in
     for (i = 0; i < numEntries; i++) {
	 Remove(p[i]);
         cvAssert(!this->IsInList(p[i]));
     }
     cvAssert(this->IsEmpty());
     SanityCheck();
     delete iterator;
}

//----------------------------------------------------------------------
// SortedList::SanityCheck
//      Test whether this is still a legal sorted list.
//
//	Test: is the list sorted?
//----------------------------------------------------------------------

template <class T>
void
cvSortedList<T>::SanityCheck() const
{
    cvLispListElement<T> *prev, *ptr;

    cvLispList<T>::SanityCheck();
    if (this->first != this->last) {
      for (prev = this->first, ptr = (this->first)->next; ptr != NULL;
						prev = ptr, ptr = ptr->next) {
            cvAssert(compare(prev->item, ptr->item) <= 0);
        }
    }
}

//----------------------------------------------------------------------
// SortedList::SelfTest
//      Test whether this module is working.
//----------------------------------------------------------------------

template <class T>
void
cvSortedList<T>::SelfTest(T *p, int numEntries)
{
    int i;
    T *q = new T[numEntries];

    cvLispList<T>::SelfTest(p, numEntries);

    for (i = 0; i < numEntries; i++) {
	 Insert(p[i]);
         cvAssert(this->IsInList(p[i]));
     }
     SanityCheck();

     // should be able to get out everything we put in
     for (i = 0; i < numEntries; i++) {
	 q[i] = this->RemoveFront();
         cvAssert(!this->IsInList(q[i]));
     }
     cvAssert(this->IsEmpty());

     // make sure everything came out in the right order
     for (i = 0; i < (numEntries - 1); i++) {
         cvAssert(compare(q[i], q[i + 1]) <= 0);
     }
     SanityCheck();

     delete q;
}

#endif // BERKELEY_LIST_H
