# Use SimpleElastix to register surface meshes

# Path to the ct/mr images or segmentation results
p_id=WS01
image_dir=./01-Images/$p_id
# Path to the mask file
mask_dir=./02-Segmnts/$p_id
# Path to the unregistered surface mesh
surface_dir=./03-Surfaces/$p_id
# Path to the registered surface meshes
output_dir=./04-SurfReg/$p_id

# Phase ID of the surface mesh used as the registration source
start_phase=8

# Registration with SimpleElastix
python modeling/elastix_main.py \
    --image_dir $mask_dir \
    --mask_dir $mask_dir \
    --output_dir $output_dir \
    --start_phase $start_phase \
    --surface_dir $surface_dir \
    --edge_size 3.5
