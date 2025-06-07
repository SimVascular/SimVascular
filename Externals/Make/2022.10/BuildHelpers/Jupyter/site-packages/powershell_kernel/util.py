import os

def get_powershell():
    # nt is a name for windows
    return 'powershell' if os.name == 'nt' else 'pwsh'
