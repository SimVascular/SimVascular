import tensorflow as tf
import numpy as np

def l2_reg(lam):
    l = 0.0
    train_vars = [v for v in tf.trainable_variables() if 'W' in v.name]
    N = len(train_vars)
    for v in train_vars:
        l += (lam/N)*tf.reduce_mean(tf.square(v))
    return l

def conv2D(x, dims=[3,3], nfilters=32, strides=[1,1],
           init=1e-3, padding='SAME', activation=tf.identity, scope='conv2d', reuse=False):
    """
    args:
        x, (tf tensor), tensor with shape (batch,width,height,channels)
        dims, (list), size of convolution filters
        filters, (int), number of filters used
        strides, (list), number of steps convolutions slide
        std, (float/string), std of weight initialization, 'xavier' for xavier
            initialization
        padding, (string), 'SAME' or 'VALID' determines if input should be padded
            to keep output dimensions the same or not
        activation, (tf function), tensorflow activation function, e.g. tf.nn.relu
        scope, (string), scope under which to store variables
        reuse, (boolean), whether we want to reuse variables that have already
            been created (i.e. reuse an earilier layer)
    returns:
        a, (tf tensor), the output of the convolution layer, has size
            (batch, new_width , new_height , filters)
    """
    with tf.variable_scope(scope,reuse=reuse):

        s = x.get_shape().as_list()

        shape = dims +[s[3],nfilters]

        if init=='xavier':
            init = np.sqrt(1.0/(dims[0]*dims[1]*s[3]))

        W = tf.Variable(tf.random_normal(shape=shape,stddev=init),
            name='W')
        b = tf.Variable(tf.zeros([nfilters]), name='b')

        o = tf.nn.convolution(x, W, padding, strides=strides)

        o = o+b

        a = activation(o)

        return a

def fullyConnected(x,output_units=100,activation=tf.identity,std=1e-3,
                  scope='fc',reuse=False):
    """
    args:
        x, (tf tensor), tensor with shape (batch,width,height,channels)
        std, (float/string), std of weight initialization, 'xavier' for xavier
            initialization
        output_units,(int), number of output units for the layer
        activation, (tf function), tensorflow activation function, e.g. tf.nn.relu
        scope, (string), scope under which to store variables
        reuse, (boolean), whether we want to reuse variables that have already
            been created (i.e. reuse an earilier layer)
    returns:
        a, (tf tensor), the output of the fullyConnected layer, has size
            (batch, output_units)
    """
    with tf.variable_scope(scope,reuse=reuse):

        s = x.get_shape().as_list()
        print(s)
        shape = [s[1],output_units]

        if std=='xavier':
            std = np.sqrt(1.0/(shape[0]))

        W = tf.get_variable('W',shape=shape,initializer=tf.random_normal_initializer(0.0,std))
        b = tf.get_variable("b",shape=shape[1],initializer=tf.constant_initializer(0.0))

        h = tf.matmul(x,W)+b
        a = activation(h)
        return a

def repeat(x,axis,repeat):
    s = x.get_shape().as_list()
    splits = tf.split(value=x,num_or_size_splits=s[axis],axis=axis)
    rep = [s for s in splits for _ in range(repeat)]
    return tf.concat(rep,axis)

def resize_tensor(x,scales=[1,2,2,1]):
    out = x
    for i in range(1,len(scales)):
        out = repeat(out,i,scales[i])
    return out

def upsample2D(x, scope='upsample'):
    with tf.variable_scope(scope):
        #o = resize_tensor(x)
        s = x.get_shape().as_list()
        s = [s[1]*2,s[2]*2]
        #Defaults to bilinear
        o = tf.image.resize_images(x,s)
        return o

def resNetConv(x,nfilters_small=32,nfilters_large=128,init='xavier',activation=tf.nn.relu,scope='resconv'):
    with tf.variable_scope(scope):
        o1 = conv2D(x,dims=[1,1], nfilters=nfilters_small,init=init,activation=activation,scope='conv_1')
        o2 = conv2D(o1,dims=[3,3], nfilters=nfilters_small,init=init,activation=activation,scope='conv_2')
        o3 = conv2D(o2,dims=[1,1], nfilters=nfilters_large,init=init,activation=tf.identity,scope='conv_3')

    return activation(x+o3)

def resNetBlock(x,nlayers=10,nfilters_small=32,nfilters_large=128,init='xavier',activation=tf.nn.relu,scope='resconv'):
    with tf.variable_scope(scope):
        o = conv2D(x,dims=[1,1],nfilters=nfilters_large,init=init,activation=tf.identity,scope='projection')
        for i in range(nlayers):
            scope_ = 'res_'+str(i)
            o = resNetConv(o,nfilters_small=nfilters_small,nfilters_large=nfilters_large,
            init=init,activation=activation,scope=scope_)
    return o

def resNet(x, nlayers_before=10, nlayers_after=10, nfilters=32, nfilters_large=128, output_filters=10,
    init='xavier', activation=tf.nn.relu, scope='resnet'):
    with tf.variable_scope(scope):
        encoding = resNetBlock(x,nlayers=nlayers_before,nfilters_small=nfilters,nfilters_large=nfilters_large,
        init=init,activation=activation,scope='resblock_before')

        output = resNetBlock(encoding,nlayers=nlayers_after,nfilters_small=nfilters,nfilters_large=nfilters_large,
        init=init,activation=activation,scope='resblock_after')

    yhat = conv2D(output, dims=[1,1], activation=tf.identity, nfilters=output_filters, init=init)
    yclass = tf.sigmoid(yhat)
    return yclass,yhat,output,encoding

def I2INetBlock(x,nfilters=32,init='xavier',activation=tf.nn.relu, num=2,scope='i2i'):
    with tf.variable_scope(scope):
        o1 = conv2D(x,nfilters=nfilters,init=init,activation=activation)
        o2 = conv2D(o1,nfilters=nfilters,init=init,activation=activation)
        if num==3:
            o2 = conv2D(o1,nfilters=nfilters,init=init,activation=activation)
        o3 = tf.nn.pool(o2,[2,2],strides=[2,2],pooling_type='MAX',padding='SAME',name='pool')
    return o3,o2

def I2INetUpsampleBlock(x,y,n1=64,n2=32,init='xavier',activation=tf.nn.relu,scope='ublock'):
    with tf.variable_scope(scope):
        y = upsample2D(y)
        x = tf.concat([x,y],axis=3)
        o = conv2D(x,dims=[1,1],nfilters=n1,init=init,activation=activation)
        o1 = conv2D(o,nfilters=n2,init=init,activation=activation)
        o2 = conv2D(o1,nfilters=n2,init=init,activation=activation)

        return o2

def I2INet(x, nfilters=32, init='xavier', activation=tf.nn.relu, output_filters=1):

    d1,a1 = I2INetBlock(x,nfilters=nfilters,init=init,activation=activation,scope='block1')
    d2,a2 = I2INetBlock(d1,nfilters=4*nfilters,init=init,activation=activation, scope='block2')
    d3,a3 = I2INetBlock(d2,nfilters=8*nfilters,init=init,activation=activation, scope='block3')
    d4,a4 = I2INetBlock(d3,nfilters=16*nfilters,init=init,activation=activation, scope='block4')

    o4 = I2INetUpsampleBlock(a3,a4,n1=16*nfilters,n2=8*nfilters,init=init,activation=activation, scope='ublock1')
    o3 = I2INetUpsampleBlock(a2,o4,n1=8*nfilters,n2=4*nfilters,init=init,activation=activation, scope='ublock2')
    o2 = I2INetUpsampleBlock(a1,o3,n1=4*nfilters,n2=nfilters,init=init,activation=activation, scope='ublock3')

    out = conv2D(o2,nfilters=output_filters,activation=tf.identity,init=init)
    out_class = tf.sigmoid(out)
    return out_class,out,o3,o4

def unetBlock(x,nfilters=32,scope='unet3d',init=1e-3,activation=tf.nn.relu):
    with tf.variable_scope(scope):
        o1 = conv2D(x,nfilters=nfilters,init=init,activation=activation)
        o2 = conv2D(o1,nfilters=2*nfilters,init=init,activation=activation)
        o3 = tf.nn.pool(o2,[2,2],strides=[2,2],pooling_type='MAX',padding='SAME',name='pool')
    return o3,o2

def unetUpsampleBlock(x,y,activation=tf.nn.relu,init=1e-3,nfilters=32,scope='unet3dupsample'):
    with tf.variable_scope(scope):
        o = upsample2D(y)
        o = tf.concat([o,x],axis=3)
        o = conv2D(o,nfilters=nfilters,init=init,activation=activation)
        o = conv2D(o,nfilters=nfilters,init=init,activation=activation)
        return o

def UNet(x,activation=tf.nn.relu,nfilters=32,init=1e-3):
    o1_down,o1 = unetBlock(x,nfilters=nfilters,scope='layer1',init=init,activation=activation)
    print( o1,o1_down)
    o2_down,o2 = unetBlock(o1_down,nfilters=2*nfilters,scope='layer2',init=init,activation=activation)
    print( o2,o2_down)
    o3_down,o3 = unetBlock(o2_down,nfilters=4*nfilters,scope='layer3',init=init,activation=activation)
    print( o3,o3_down)
    o4_down,o4 = unetBlock(o3_down,nfilters=8*nfilters,scope='layer4',init=init,activation=activation)
    print( o4,o4_down)

    a_3 = unetUpsampleBlock(o3,o4,nfilters=8*nfilters,scope='o_layer3',init=init,activation=activation)
    print( a_3)
    a_2 = unetUpsampleBlock(o2,a_3,nfilters=4*nfilters,scope='o_layer2',init=init,activation=activation)
    print( a_2)
    a_1 = unetUpsampleBlock(o1,a_2,nfilters=2*nfilters,scope='o_layer1',init=init,activation=activation)
    print( a_1)
    yhat = conv2D(a_1,activation=tf.identity,dims=[1,1],nfilters=1,scope='yhat',init=init)
    print( yhat)
    yclass = tf.sigmoid(yhat)
    print( yclass)
    return yclass, yhat

def I2INetFC(x, nfilters=32, init='xavier', activation=tf.nn.relu, output_filters=1):
    Nbatch = tf.shape(x)[0]
    CROP_DIMS = x.get_shape().as_list()[1]

    yclass_,yhat_,o3,o4 = I2INet(x,nfilters=nfilters,
        activation=activation,init=init)

    #I2INetFC
    y_vec = tf.reshape(yhat_, (Nbatch,CROP_DIMS**2))

    sp = fullyConnected(y_vec,CROP_DIMS,activation, std=init, scope='sp1')
    sp = fullyConnected(y_vec,CROP_DIMS**2,activation, std=init, scope='sp2')
    sp = tf.reshape(sp, (Nbatch,CROP_DIMS,CROP_DIMS,1))

    y_sp = conv2D(sp, nfilters=nfilters,
        activation=activation,init=init, scope='sp3')
    y_sp_1 = conv2D(y_sp, nfilters=nfilters,
        activation=activation, init=init,scope='sp4')
    y_sp_2 = conv2D(y_sp_1, nfilters=nfilters,
        activation=activation, init=init,scope='sp5')

    yhat = conv2D(y_sp_2, nfilters=1, activation=tf.identity, init=init,scope='sp6')

    yclass = tf.sigmoid(yhat)

    return yclass, yhat, yclass_, yhat_

def multitaskNet(x, nfilters=32, init='xavier', activation=tf.nn.relu, output_filters=1,
    hidden_size=300,num_contour_points=20):

    Nbatch   = tf.shape(x)[0]
    CROP_DIMS = x.get_shape().as_list()[1]

    yclass,yhat,o3,o4 = I2INet(x,nfilters=nfilters,activation=activation,init=init,
                                      output_filters=output_filters)

    ##########################
    # Encoder layer
    ##########################
    ydown = tf.nn.pool(yclass,[2,2],strides=[2,2],pooling_type='MAX',padding='SAME',name='pool')

    y_vec = tf.reshape(ydown, (Nbatch,(output_filters*CROP_DIMS**2)/4))

    encoding = fullyConnected(y_vec, 2*hidden_size,activation, std='xavier', scope='sp1')
    encoding = fullyConnected(encoding, 2*hidden_size,activation, std='xavier', scope='sp1_1')
    encoding = fullyConnected(encoding, hidden_size,activation, std='xavier', scope='sp1_2')

    ##########################
    sp = fullyConnected(encoding, output_filters*CROP_DIMS**2,activation, std='xavier', scope='sp2')
    sp = tf.reshape(sp, (Nbatch,CROP_DIMS,CROP_DIMS,output_filters))

    y_sp   = conv2D(sp, nfilters=nfilters, activation=activation,init=init, scope='sp3')
    y_sp_1 = conv2D(y_sp, nfilters=nfilters, activation=activation, init=init, scope='sp4')

    #Yc prediction
    yc_hat   = conv2D(y_sp_1, nfilters=output_filters, activation=tf.identity, init=init,scope='sp_yc')

    yc_class = tf.sigmoid(yc_hat)

    ############################
    #C prediction
    ############################
    c_sp = fullyConnected(encoding, output_filters*num_contour_points, tf.tanh, std='xavier', scope='sp2_c')

    c_hat = tf.reshape(c_sp,(Nbatch,output_filters,num_contour_points))

    ############################
    #B prediction
    ############################
    p_sp = fullyConnected(encoding, output_filters*2, tf.tanh, std='xavier', scope='sp2_b')

    p_hat = tf.reshape(p_sp,(Nbatch,output_filters,2))

    return yclass,yhat, yc_class, yc_hat,c_hat,p_hat

def class_balanced_cross_entropy(pred, label, name='cross_entropy_loss'):
    """
    The class-balanced cross entropy loss,
    as in `Holistically-Nested Edge Detection
    <http://arxiv.org/abs/1504.06375>`_.
    Args:
        pred: of shape (b, ...). the predictions in [0,1].
        label: of the same shape. the ground truth in {0,1}.
    Returns:
        class-balanced cross entropy loss.
    """
    with tf.name_scope('class_balanced_cross_entropy'):
        z = batch_flatten(pred)
        y = tf.cast(batch_flatten(label), tf.float32)

        count_neg = tf.reduce_sum(1. - y)
        count_pos = tf.reduce_sum(y)
        beta = count_neg / (count_neg + count_pos)

        eps = 1e-12
        loss_pos = -beta * tf.reduce_mean(y * tf.log(z + eps))
        loss_neg = (1. - beta) * tf.reduce_mean((1. - y) * tf.log(1. - z + eps))
    cost = tf.subtract(loss_pos, loss_neg, name=name)
    return cost


def class_balanced_sigmoid_cross_entropy(logits, label, name='cross_entropy_loss'):
    """
    This function accepts logits rather than predictions, and is more numerically stable than
    :func:`class_balanced_cross_entropy`.
    """
    with tf.name_scope('class_balanced_sigmoid_cross_entropy'):
        y = tf.cast(label, tf.float32)

        count_neg = tf.reduce_sum(1. - y)
        count_pos = tf.reduce_sum(y)
        beta = count_neg / (count_neg + count_pos)

        pos_weight = beta / (1 - beta)
        cost = tf.nn.weighted_cross_entropy_with_logits(logits=logits, targets=y, pos_weight=pos_weight)
        cost = tf.reduce_mean(cost * (1 - beta))
        zero = tf.equal(count_pos, 0.0)
    return tf.where(zero, 0.0, cost, name=name)

def masked_loss_2D(logits, label, radius=30):
    with tf.name_scope('mask_loss'):
        w = tf.ones(shape=[radius,radius,1],dtype=tf.float32)

        y_dilated = tf.nn.dilation2d(label,w,strides=[1,1,1,1],rates=[1,1,1,1],
            padding='SAME')-1

        loss = tf.nn.sigmoid_cross_entropy_with_logits(logits=logits, labels=label)
        loss = tf.multiply(y_dilated,loss)
        loss = tf.reduce_sum(loss)/tf.reduce_sum(y_dilated)

        return loss

def masked_loss_3D(logits, label, radius=30):
    with tf.name_scope('mask_loss'):
        s = tf.shape(logits)
        channels = s[3]
        w = tf.ones(shape=[radius,radius,channels],dtype=tf.float32)

        label_reduced  = label[:,:,:,:,0]
        logits_reduced = logits[:,:,:,:,0]

        y_dilated = tf.nn.dilation2d(label_reduced,w,strides=[1,1,1,1],rates=[1,1,1,1],
            padding='SAME')-1

        loss = tf.nn.sigmoid_cross_entropy_with_logits(logits=logits_reduced,
            labels=label_reduced)
        loss = tf.multiply(y_dilated,loss)
        loss = tf.reduce_sum(loss)/tf.reduce_sum(y_dilated)

        return loss

def inception(x, c1x1=64, c3x3_reduce=96, c3x3=128, c5x5_reduce=16,
    c5x5=32, c1x1_pool=32, activation=tf.nn.relu, init='xavier', scope="inception"):

    with tf.variable_scope(scope):
        #1x1
        o_1x1 = conv2D(x, nfilters=c1x1, dims=[1,1], activation=activation,
            init=init, scope='1x1')

        #3x3_reduce
        o_3x3_r = conv2D(x, nfilters=c3x3_reduce, dims=[1,1], activation=activation,
            init=init, scope='3x3_reduce')

        o_3x3 = conv2D(o_3x3_r, nfilters=c3x3, dims=[3,3], activation=activation,
            init=init, scope='3x3')

        #5x5_reduce
        o_5x5_r = conv2D(x, nfilters=c5x5_reduce, dims=[1,1], activation=activation,
            init=init, scope='5x5_reduce')

        o_5x5 = conv2D(o_5x5_r, nfilters=c5x5, dims=[5,5], activation=activation,
            init=init, scope='5x5')

        #1x1 pool proj
        o_pool = tf.nn.pool(x, [3,3], "MAX", "SAME", strides=[1,1])

        o_1x1_p = conv2D(o_pool, nfilters=c1x1_pool, dims=[1,1], activation=activation,
            init=init, scope='1x1_pool')

        #concat
        o = tf.concat([o_1x1, o_3x3, o_5x5, o_1x1_p],axis=3)

        return o

def GoogleNet(x, activation=tf.nn.relu, init='xavier', drop=0.7,
    scope='googlenet', output_size=1000):
    #input branch
    with tf.variable_scope(scope):
        o = conv2D(x, nfilters=64, dims=[7,7], strides=[1,1], activation=activation,
            init=init, scope='input_7x7')

        o = tf.nn.pool(o, [3,3], "MAX", "VALID", strides=[2,2])

        print("pool 1", o)

        o = conv2D(o, nfilters=64, dims=[1,1], strides=[1,1], activation=activation,
            init=init, scope='input_1x1_r')

        o = conv2D(o, nfilters=192, dims=[3,3], strides=[1,1], activation=activation,
            init=init, scope='input_3x3')

        o = tf.nn.pool(o, [3,3], "MAX", "VALID", strides=[2,2])

        print("pool_2", o)

        #inception layers 3
        o = inception(o, c1x1=64, c3x3_reduce=96, c3x3=128, c5x5_reduce=16,
            c5x5=32, c1x1_pool=32, activation=activation, init=init, scope="inception_3a")

        o = inception(o, c1x1=128, c3x3_reduce=128, c3x3=192, c5x5_reduce=32,
            c5x5=96, c1x1_pool=64, activation=activation, init=init, scope="inception_3b")

        #inception layers 4
        o = tf.nn.pool(o, [3,3], "MAX", "VALID", strides=[2,2])

        print("pool 3", o)

        o = inception(o, c1x1=192, c3x3_reduce=96, c3x3=208, c5x5_reduce=16,
            c5x5=48, c1x1_pool=64, activation=activation, init=init, scope="inception_4a")

        o = inception(o, c1x1=160, c3x3_reduce=112, c3x3=224, c5x5_reduce=24,
            c5x5=64, c1x1_pool=64, activation=activation, init=init, scope="inception_4b")

        o = inception(o, c1x1=128, c3x3_reduce=128, c3x3=256, c5x5_reduce=24,
            c5x5=64, c1x1_pool=64, activation=activation, init=init, scope="inception_4c")

        o = inception(o, c1x1=112, c3x3_reduce=144, c3x3=288, c5x5_reduce=32,
            c5x5=64, c1x1_pool=64, activation=activation, init=init, scope="inception_4d")

        o = inception(o, c1x1=256, c3x3_reduce=160, c3x3=320, c5x5_reduce=32,
            c5x5=128, c1x1_pool=129, activation=activation, init=init, scope="inception_4e")

        #side output 1
        o_side = tf.nn.pool(o, [5,5], "AVG", "VALID", strides=[3,3])

        print("pool side", o_side)

        o_side = conv2D(o_side, nfilters=128, dims=[1,1], strides=[1,1], activation=activation,
            init=init, scope='side_1x1')

        s_side = o_side.get_shape().as_list()

        o_side_vec = tf.reshape(o_side,shape=[-1,s_side[1]*s_side[2]*s_side[3]])

        o_side = fullyConnected(o_side_vec, output_units=1024, activation=activation,
            std='xavier', scope='side_fc1')

        o_side = tf.nn.dropout(o_side, keep_prob=1.0-drop)

        o_side = fullyConnected(o_side, output_units=output_size, activation=tf.identity,
            std='xavier', scope='side_output')

        #inception layers 5
        o = tf.nn.pool(o, [3,3], "MAX", "VALID", strides=[2,2])

        print("pool 5", o)

        o = inception(o, c1x1=256, c3x3_reduce=160, c3x3=320, c5x5_reduce=32,
            c5x5=128, c1x1_pool=128, activation=activation, init=init, scope="inception_5a")

        o = inception(o, c1x1=384, c3x3_reduce=192, c3x3=384, c5x5_reduce=48,
            c5x5=128, c1x1_pool=128, activation=activation, init=init, scope="inception_5b")

        #final output
        o = tf.nn.pool(o, [7,7], "AVG", "VALID", strides=[1,1])

        print("pool final", o)

        s = o.get_shape().as_list()

        o = tf.reshape(o,shape=[-1,s[1]*s[2]*s[3]])

        o = tf.nn.dropout(o, keep_prob=0.6)

        o = fullyConnected(o, output_units=1024, activation=activation,
            std='xavier', scope='output_fc1')

        o = fullyConnected(o, output_units=output_size, activation=tf.identity,
            std='xavier', scope='output')

        return o, o_side
