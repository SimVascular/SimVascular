/* copyright (C) 2001-2004 Jean-Luc Fontaine (mailto:jfontain@free.fr)
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 */

/* Provide Python interpreters accessible from Tcl as a package named "tclpython". */

#include <Python.h>
#include <tcl.h>
#include <cStringIO.h>
#include "tclpython.h"

//#include "pythonModules.h"

static unsigned existingInterpreters = 0;
static struct Tcl_HashTable threadStates;
static struct Tcl_HashTable dictionaries;
static int newIdentifier;
#ifdef WITH_THREAD
static PyThreadState *globalState = 0;
#endif

static Tcl_Interp *mainInterpreter; /* needed for Tcl evaluation from Python side */
static Tcl_ThreadId mainThread;     /* needed for Python threads, 0 if Tcl core is not thread-enabled */

static int pythonInterpreter(ClientData clientData, Tcl_Interp *interpreter, int numberOfArguments, Tcl_Obj * CONST arguments[])
{
    int identifier;
    PyObject *output;
    PyObject *message;
    PyObject *result;
    PyObject *globals;
    char *string = 0;
    int length;
    Tcl_Obj *object;
    struct Tcl_HashEntry *entry;
    unsigned evaluate;
#ifdef WITH_THREAD
    PyThreadState *state;
#endif

    if (numberOfArguments != 3) {
        Tcl_WrongNumArgs(interpreter, 1, arguments, "eval script");
        return TCL_ERROR;
    }
    string = Tcl_GetString(arguments[1]);
    evaluate = (strcmp(string, "eval") == 0);                                                       /* else the action is execute */
    if (!evaluate && (strcmp(string, "exec") != 0)) {
        object = Tcl_NewObj();
        Tcl_AppendStringsToObj(object, "bad option \"", string, "\": must be eval or exec", 0);
        Tcl_SetObjResult(interpreter, object);
        return TCL_ERROR;
    }
    identifier = atoi(Tcl_GetString(arguments[0]) + 6);                              /* interpreter and command name is "pythonN" */
    entry = Tcl_FindHashEntry(&threadStates, (ClientData)identifier);
    if (entry == 0) {
        object = Tcl_NewObj();
        Tcl_AppendStringsToObj(
            object, "invalid interpreter \"", Tcl_GetString(arguments[0]), "\": internal error, please report to tclpython author",
            0
        );
        Tcl_SetObjResult(interpreter, object);
        return TCL_ERROR;
    }
    globals = Tcl_GetHashValue(Tcl_FindHashEntry(&dictionaries, (ClientData)identifier));
#ifdef WITH_THREAD
    state = Tcl_GetHashValue(entry);
    PyEval_RestoreThread(state);                              /* acquire the global interpreter lock and make this thread current */
#endif
    /* choose start token depending on whether this is an evaluation or an execution: */
    result = PyRun_String(Tcl_GetString(arguments[2]), (evaluate? Py_eval_input: Py_file_input), globals, globals);
    if (result == 0) {                                                                                        /* an error occured */
        output = PycStringIO->NewOutput(1024);               /* use a reasonable initial size but big enough to handle most cases */
        PySys_SetObject("stderr", output);                                                /* capture all interpreter error output */
        PyErr_Print();                                            /* so that error is printed on standard error, redirected above */
        message = PycStringIO->cgetvalue(output);
        string = PyString_AsString(message);
        length = PyString_Size(message);
        if ((length > 0) && (string[length - 1] == '\n')) length--;              /* eventually remove trailing new line character */
        object = Tcl_NewObj();
        Tcl_AppendStringsToObj(object, Tcl_GetString(arguments[0]), ": ", 0);                    /* identify interpreter in error */
        Tcl_AppendObjToObj(object, Tcl_NewStringObj(string, length));
        Py_DECREF(output);
    } else {
        if (evaluate) {
            string = PyString_AsString(PyObject_Str(result));
            object = Tcl_NewStringObj(string, -1);                                                    /* return evaluation result */
        } else                                                                                                         /* execute */
            object = Tcl_NewObj();                                                   /* always return an empty result or an error */
        Py_DECREF(result);
    }
#ifdef WITH_THREAD
    PyEval_SaveThread();                        /* eventually restore the previous thread and release the global interpreter lock */
#endif
    Tcl_SetObjResult(interpreter, object);
    return(result == 0? TCL_ERROR: TCL_OK);
}

Tcl_Interp *tclInterpreter(CONST char *name)                           /* public function for use in extensions to this extension */
{
    int identifier;

    if ((sscanf(name, "tcl%u", &identifier) == 0) || (identifier != 0)) {
        return 0;                                                                                                 /* invalid name */
    } else {
        return mainInterpreter;                                                                     /* sole available interpreter */
    }
}

static int tclEvaluate(CONST char *name, CONST char *script, char **string, int *length)              /* returns true if no error */
{
    Tcl_Interp *interpreter;
    int result;
    Tcl_Obj *object;

    interpreter = tclInterpreter(name);
    if (interpreter == 0) {
        object = Tcl_NewObj();
        Tcl_AppendStringsToObj(object, "invalid Tcl interpreter name: ", name, 0);
        *string = Tcl_GetStringFromObj(object, length);
        return 0;
    } else if (CURRENTTHREAD == mainThread) {                                                                /* non threaded code */
        Tcl_Preserve(interpreter);
        result = Tcl_EvalEx(interpreter, script, -1, TCL_EVAL_DIRECT | TCL_EVAL_GLOBAL);
        *string = Tcl_GetStringFromObj(Tcl_GetObjResult(interpreter), length);
        Tcl_Release(interpreter);
        return (result == TCL_OK);
    } else {                               /* threaded code: function like Tcl thread extension send command in asynchronous mode */
        tclSendThread(mainThread, interpreter, script);               /* let the interpreter in the main thread evaluate the code */
        *string = 0;                                                                                       /* nothing is returned */
        return 1;                                                       /* errors, if any, are reported on standard error channel */
    }
}

static PyObject *pythonTclEvaluate(PyObject *self, PyObject *args)
{
    CONST char *script;
    char *result;
    int length;

    if (!PyArg_ParseTuple(args, "s", &script))
        return 0;
    length = strlen(script);
    if (!tclEvaluate("tcl0", script, &result, &length)) {
        PyErr_SetString(PyExc_RuntimeError, result);
    }
    return Py_BuildValue("s", result);
}

static PyMethodDef tclMethods[] = {
    {"eval", pythonTclEvaluate, METH_VARARGS, "Evaluate a Tcl script."},
    {0, 0, 0, 0}                                                                                                      /* sentinel */
};

static int newInterpreter(Tcl_Interp *interpreter)
{
    int identifier;
    Tcl_Obj *object;
    int created;
#ifdef WITH_THREAD
    PyThreadState *state;
#endif
    PyObject *tcl;

    identifier = newIdentifier;
#ifndef WITH_THREAD
    if (existingInterpreters > 0) {
        Tcl_SetResult(
            interpreter,
            "cannot create several concurrent Python interpreters\n(Python library was compiled without thread support)",
            TCL_STATIC
        );
        return TCL_ERROR;
    } else {
        Py_Initialize();                                                                           /* initialize main interpreter */
        PycString_IMPORT;
    }
    Tcl_SetHashValue(Tcl_CreateHashEntry(&threadStates, (ClientData)identifier, &created), 0);
#else
    if (existingInterpreters == 0) {
        Py_Initialize();                                                                           /* initialize main interpreter */
	//fprintf(stdout,"Build Info: %s\n",Py_GetBuildInfo());
	printf("  %-12s %s\n", "Python:",
	       Py_GetVersion());
	PyEval_InitThreads();                                               /* initialize and acquire the global interpreter lock */
        PycString_IMPORT;
        globalState = PyThreadState_Swap(0);                                                            /* save the global thread */
	//
    } else {
        PyEval_AcquireLock();                                           /* needed in order to be able to create a new interpreter */
    }
    if (PycStringIO == 0) {                                              /* make sure string input/output is properly initialized */
        Tcl_SetResult(interpreter, "fatal error: could not initialize Python string input/output module", TCL_STATIC);
        return TCL_ERROR;
    }
    state = Py_NewInterpreter();          /* hangs here if automatic 'import site' on a new thread is allowed (set Py_NoSiteFlag) */
    if (state == 0) {
        PyEval_ReleaseLock();
        Tcl_SetResult(interpreter, "could not create a new interpreter: please report to tclpython author", TCL_STATIC);
        return TCL_ERROR;
    }
    PyEval_ReleaseLock();                                                                  /* release the global interpreter lock */
    Tcl_SetHashValue(Tcl_CreateHashEntry(&threadStates, (ClientData)identifier, &created), state);
#endif
    Tcl_SetHashValue(
        Tcl_CreateHashEntry(&dictionaries, (ClientData)identifier, &created), PyModule_GetDict(PyImport_AddModule("__main__"))
    );
    object = Tcl_NewStringObj("python", -1);
    Tcl_AppendObjToObj(object, Tcl_NewIntObj(identifier));                          /* return "pythonN" as interpreter identifier */
    Tcl_SetObjResult(interpreter, object);
    Tcl_CreateObjCommand(interpreter, Tcl_GetString(object), pythonInterpreter, 0, 0);      /* create command for new interperter */
#ifdef WITH_THREAD
    newIdentifier++;
#endif
    existingInterpreters++;
    tcl = Py_InitModule("tcl", tclMethods);                                   /* add a new 'tcl' module to the python interpreter */
    Py_INCREF(tcl);
    PyModule_AddObject(PyImport_AddModule("__builtin__"), "tcl", tcl);

	//-------------------------------IMPORT MODULES--------------------//
    //PyObject *pythonC;
    //pythonC = Py_InitModule("pythonc", pythonc_methods);
    //Py_INCREF(pythonC);
    //PyModule_AddObject(PyImport_AddModule("__buildin__"), "pythonc", pythonC);
	//-----------------------------------------------------------------//
    return TCL_OK;
}

static int deleteInterpreters(Tcl_Interp *interpreter, int numberOfArguments, Tcl_Obj * CONST arguments[])
{
    int index;
    char *name;
    int identifier;
    struct Tcl_HashEntry *entry;
    Tcl_Obj *object;
#ifdef WITH_THREAD
    PyThreadState *state;
#endif

    for (index = 0; index < numberOfArguments; index++) {
        name = Tcl_GetString(arguments[index]);                                                  /* interpreter name is "pythonN" */
        entry = 0;
        if (sscanf(name, "python%u", &identifier) == 1) {
            identifier = atoi(name + 6);
            entry = Tcl_FindHashEntry(&threadStates, (ClientData)identifier);
        }
        if (entry == 0) {
            object = Tcl_NewObj();
            Tcl_AppendStringsToObj(object, "invalid interpreter \"", name, 0);
            Tcl_SetObjResult(interpreter, object);
            return TCL_ERROR;
        }
#ifdef WITH_THREAD
        state = Tcl_GetHashValue(entry);
        PyEval_AcquireThread(state);                          /* acquire the global interpreter lock and make this thread current */
        Py_EndInterpreter(state);
        PyEval_ReleaseLock();
#endif
        Tcl_DeleteHashEntry(entry);
        Tcl_DeleteHashEntry(Tcl_FindHashEntry(&dictionaries, (ClientData)identifier));
        existingInterpreters--;
        if (existingInterpreters == 0) {                                                         /* no remaining sub-interpreters */
#ifdef WITH_THREAD
            PyEval_AcquireThread(globalState);                                                    /* required before finalization */
            globalState = 0;
#endif
            Py_Finalize();                                                                                 /* clean everything up */
        }
    }
    return TCL_OK;
}

static int command(ClientData clientData, Tcl_Interp *interpreter, int numberOfArguments, Tcl_Obj * CONST arguments[])
{
    char *command;
    unsigned new;
    unsigned delete;
    Tcl_Obj *object;

    if (numberOfArguments < 2) {
        Tcl_WrongNumArgs(interpreter, 1, arguments, "new|delete ?interp interp ...?");
        return TCL_ERROR;
    }
    command = Tcl_GetString(arguments[1]);
    new = (strcmp(command, "new") == 0);
    delete = (strcmp(command, "delete") == 0);
    if (!new && !delete) {
        object = Tcl_NewObj();
        Tcl_AppendStringsToObj(object, "bad option \"", command, "\": must be new or delete", 0);
        Tcl_SetObjResult(interpreter, object);
        return TCL_ERROR;
    }
    if (new) {
        if (numberOfArguments != 2) {
            Tcl_WrongNumArgs(interpreter, 1, arguments, "new");
            return TCL_ERROR;
        } else {
            return newInterpreter(interpreter);
        }
    }
    if (delete) {
#ifdef WITH_THREAD
        if (numberOfArguments < 3) {
            Tcl_WrongNumArgs(interpreter, 1, arguments, "delete ?interp interp ...?");
#else
        if (numberOfArguments != 3) {                                                        /* there can be one interpreter only */
            Tcl_WrongNumArgs(interpreter, 1, arguments, "delete interp");
#endif
            return TCL_ERROR;
        } else {
            return deleteInterpreters(interpreter, numberOfArguments -2, arguments + 2);
        }
    }
    return TCL_ERROR;                                                                                            /* never reached */
}

#ifdef WIN32
/* George Petasis, 21 Feb 2006:
 * Under Visual C++, functions exported from DLLs must be declared
 * with __declspec(dllexport) */
#undef TCL_STORAGE_CLASS
#define TCL_STORAGE_CLASS DLLEXPORT
#endif

int Tclpython_Init(Tcl_Interp *interpreter)
{
    Tcl_InitHashTable(&threadStates, TCL_ONE_WORD_KEYS);
    Tcl_InitHashTable(&dictionaries, TCL_ONE_WORD_KEYS);
    mainInterpreter = interpreter;
    mainThread = CURRENTTHREAD;
    newIdentifier = 0;
    Tcl_CreateObjCommand(interpreter, "::python::interp", command, 0, 0);
    Py_NoSiteFlag = 1;                      /* suppress automatic 'import site' to prevent interpreter from hanging on new thread */
    return Tcl_PkgProvide(interpreter, "tclpython", "4.1");
}
