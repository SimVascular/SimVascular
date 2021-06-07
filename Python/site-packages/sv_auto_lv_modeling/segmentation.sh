patient_id=WS01
image_dir=01-Images
output_dir=02-Segmnts
weight_dir=./Weights

sv_python_dir=/usr/local/bin
${sv_python_dir}/simvascular --python -- ./segmentation/prediction.py \
    --pid $patient_id \
    --image $image_dir \
    --output $output_dir \
    --model $weight_dir \
    --view  0 1 2 \
    --modality ct
