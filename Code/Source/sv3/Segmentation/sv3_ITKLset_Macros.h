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

#ifndef CVMACROS_H_
#define CVMACROS_H_

#define cvGetRepoObjMacro(name,type)																\
		virtual inline int Get##name(type **value){ 											\
			if(m_cv##name == NULL){ 															\
                                return SV_ERROR; 																		\
			} else { 																			\
				*value = m_cv##name;															\
                                return SV_OK; 																		\
			} 																					\
		}																						\

#define cvSetRepoObjMacro(name,cvtype,baseType)													\
		virtual inline int Set##name(cvtype *value){ 											\
			if ( m_cv##name != NULL ) { 														\
				delete m_cv##name;																\
				m_cv##name = NULL;																\
			}																					\
			m_cv##name = new cvtype ( (baseType*)value->GetVtkPtr() );							\
			m_cv##name->SetName( value->GetName() ); 											\
                        return SV_OK;																			\
		}																						\


#define cvSetMacro(name,type) \
		virtual inline void Set##name(type value){ \
			if(m_##name != value) \
			m_##name = value; \
		} \

#define cvGetMacro(name,type) \
		virtual inline type Get##name(){ \
			return this->m_##name; \
		} \


#endif /* CVMACROS_H_ */
