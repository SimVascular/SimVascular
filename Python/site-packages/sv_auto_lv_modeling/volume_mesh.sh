# Use SimVascular to construct LV volme mesh
p_id=WS01
# Path to SimVascular exectuable
sv_python_dir=/usr/local/bin

volume_mesh_script=modeling/volume_mesh_main.py

# Path to the surface meshes
input_dir=./04-SurfReg/$p_id
# Path to the outputed volume meshes
output_dir=./05-VolMesh/$p_id

# Volumetric Meshing using SimVascular
${sv_python_dir}/simvascular --python \
    -- ${volume_mesh_script} \
    --input_dir $input_dir \
    --output_dir $output_dir \
    --edge_size 2.5

