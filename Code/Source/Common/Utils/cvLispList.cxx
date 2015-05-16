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

// Adapted from the Berkeley list package, extracted from the NACHOS
// OS simulation system. 

#include "SimVascular.h" 

#define TRUE  1
#define FALSE 0


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
    Assert(IsEmpty());		// make sure list is empty
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

    Assert(!IsInList(item));
    if (IsEmpty()) {		// list is empty
	first = element;
	last = element;
    } else {			// else put it after last
	last->next = element;
	last = element;
    }
    numInList++;
    Assert(IsInList(item));
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

    Assert(!IsInList(item));
    if (IsEmpty()) {		// list is empty
	first = element;
	last = element;
    } else {			// else put it before first
	element->next = first;
	first = element;
    }
    numInList++;
    Assert(IsInList(item));
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

    Assert(!IsEmpty());

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

    Assert(IsInList(item));

    // if first item on list is match, then remove from front
    if (item == first->item) {	
        removed = RemoveFront();
        Assert(item == removed);
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
	Assert(ptr != NULL);	// should always find item!
    }
    Assert(!IsInList(item));
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

    Assert(!IsInList(item));
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
    Assert(IsInList(item));
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
	Assert((numInList == 0) && (last == NULL));
    } else if (first == last) {
	Assert((numInList == 1) && (last->next == NULL));
    } else {
        for (numFound = 1, ptr = first; ptr != last; ptr = ptr->next) {
	    numFound++;
            Assert(numFound <= numInList);	// prevent infinite loop
        }
        Assert(numFound == numInList);
        Assert(last->next == NULL);
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
    Assert(IsEmpty() && (first == NULL));
    for (; !iterator->IsDone(); iterator->Next()) {
      //	AssertNOTREACHED();	// nothing on list
    }

    for (i = 0; i < numEntries; i++) {
	 Append(p[i]);
	 Assert(IsInList(p[i]));
	 Assert(!IsEmpty());
     }
     SanityCheck();

     // should be able to get out everything we put in
     for (i = 0; i < numEntries; i++) {
	 Remove(p[i]);
         Assert(!IsInList(p[i]));
     }
     Assert(IsEmpty());
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
            Assert(compare(prev->item, ptr->item) <= 0);
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
	 Assert(IsInList(p[i]));
     }
     SanityCheck();

     // should be able to get out everything we put in
     for (i = 0; i < numEntries; i++) {
	 q[i] = this->RemoveFront();
         Assert(!IsInList(q[i]));
     }
     Assert(IsEmpty());

     // make sure everything came out in the right order
     for (i = 0; i < (numEntries - 1); i++) {
	 Assert(compare(q[i], q[i + 1]) <= 0);
     }
     SanityCheck();

     delete q;
}

