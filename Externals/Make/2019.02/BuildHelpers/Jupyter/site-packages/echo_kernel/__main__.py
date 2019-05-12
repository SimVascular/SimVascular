from ipykernel.kernelapp import IPKernelApp
from . import EchoKernel

IPKernelApp.launch_instance(kernel_class=EchoKernel)
