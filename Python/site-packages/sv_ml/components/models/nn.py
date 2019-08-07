# Copyright (c) Stanford University, The Regents of the University of
#               California, and others.
#
# All Rights Reserved.
#
# See Copyright-SimVascular.txt for additional details.
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
import os
from sv_ml.base.model import AbstractModel
import numpy as np
import tensorflow as tf
import sv_ml.modules.layers as tf_util

# import matplotlib
# matplotlib.use('Agg')
# import matplotlib.pyplot as plt

import sv_ml.modules.vessel_regression as vr

def get_batch(X,Y, batch_size=16):
    ids = np.random.choice(X.shape[0], size=batch_size)

    x   = np.array([X[i] for i in ids])
    y   = np.array([Y[i] for i in ids])

    return x,y

class Model(AbstractModel):
    def setup(self):
        self.start_iter = 0
        self.build_model()
        self.configure_trainer()
        self.finalize()

        self.losses = []
        self.iters = []

    def train_step(self,x,y):
        self.global_step = self.global_step+1

        if np.sum(np.isnan(x)) > 0: return
        if np.sum(np.isnan(y)) > 0: return

        self.sess.run(self.train_op,{self.x:x,self.y:y})

    def save(self, model_path=None):
        if model_path == None:
            model_path = os.path.join(self.config['MODEL_DIR'], self.config['MODEL_NAME'])
        else:
            model_path = os.path.join(model_path, self.config['MODEL_NAME'])
        self.saver.save(self.sess,model_path)

    def load(self, model_path=None):
        if model_path == None:
            model_path = os.path.join(self.config['MODEL_DIR'],self.config['MODEL_NAME'])
        else:
            model_path = os.path.join(model_path,self.config['MODEL_NAME'])
        self.saver.restore(self.sess, model_path)

    def predict(self,x):
        S = list(x.shape)
        if len(S) == 3:
            x_ = x.reshape([1]+S)
            return self._predict(x_)[0]
        else:
            out = []
            for i in range(S[0]):
                x_ = x[i].reshape([1]+S[1:4])
                y = self._predict(x_)[0].copy()
                out.append(y)
            return np.array(out)

    def calculate_loss(self,x,y):
        return self.sess.run(self.loss,{self.x:x,self.y:y})

    def build_model(self):
        raise RuntimeError("Abstract not implemented")

    def build_loss(self):
        self.loss = tf.reduce_mean(tf.square(self.y-self.yhat))

    def configure_trainer(self):
        LEARNING_RATE = self.config["LEARNING_RATE"]
        self.global_step = tf.Variable(0, trainable=False)
        boundaries = [2000,
                      5000,
                      10000,
                      15000,
                      150000]

        values = [LEARNING_RATE,
                  LEARNING_RATE/3,
                  LEARNING_RATE/10,
                  LEARNING_RATE/100,
                  LEARNING_RATE/1000,
                  LEARNING_RATE/10000,]

        learning_rate = tf.train.piecewise_constant(self.global_step, boundaries, values)


        self.opt = tf.train.AdamOptimizer(learning_rate)

        #self.opt = tf.train.MomentumOptimizer(learning_rate, momentum=0.9)
        self.train_op = self.opt.minimize(self.loss)

    def train(self, X,Y):
        for i in range(self.start_iter,
            self.config['TRAIN_STEPS']+self.start_iter):
            x,y = get_batch(X,Y, self.config['BATCH_SIZE'])



            self.train_step(x,y)

            if i % self.config['LOG_STEP'] == 0:
                l = self.calculate_loss(x,y)
                self.losses.append(l)
                self.iters.append(i)

                self.log(i,x,y)
                self.log(i,X[:4],Y[:4])
                self.save()

    def _predict(self,x):
        return self.sess.run(self.yhat,{self.x:x})

    def finalize(self):
        self.sess = tf.Session()
        self.sess.run(tf.global_variables_initializer())

    def log(self,i,x,y):
        pass
        # f = open(self.config['ITER_FILE'], 'w')
        # f.write(str(i))
        # f.close()
        #
        # l = self.calculate_loss(x,y)
        # yhat = self.predict(x)[0]
        #
        # print("{}: loss={}\n".format(i,l))
        # print("yhat = {}".format(yhat))
        #
        # f = open(self.config["LOG_FILE"],"a+")
        # f.write("{}: loss={}\n".format(i,l))
        # f.write("{}: yhat={}\n".format(i,yhat))
        # f.close()
        #
        # self.save()
        #
        # x_ = x[0,:,:,0]
        # y_ = y[0]
        # if self.config['DATASET'] == 'axial2d_point':
        #     ctrue = vr.point_pred_to_contour(y_)
        #     cpred = vr.point_pred_to_contour(yhat)
        #
        # else:
        #     ctrue = vr.pred_to_contour(y_)
        #     cpred = vr.pred_to_contour(yhat)
        #
        # plt.figure()
        # plt.imshow(x_,cmap='gray',extent=[-1, 1, 1, -1])
        # plt.colorbar()
        # plt.scatter(cpred[:,0], cpred[:,1], color='r', label='predicted',s=4)
        # plt.scatter(ctrue[:,0], ctrue[:,1], color='y', label='true', s=4)
        # plt.show()
        # plt.close()
        #
        # W = x_.shape[0]
        # s = int(0.25*W)
        # e = int(0.75*W)
        #
        # plt.figure()
        # plt.imshow(x_[s:e,s:e],cmap='gray',extent=[-0.5, 0.5, 0.5, -0.5])
        # plt.colorbar()
        # plt.scatter(cpred[:,0], cpred[:,1], color='r', label='predicted',s=4)
        # plt.scatter(ctrue[:,0], ctrue[:,1], color='y', label='true', s=4)
        # plt.show()
        # plt.close()
        #
        #
        # c = self.config
        # log_dir = c['RESULTS_DIR']+'/'+c['NAME']+'/log/'
        #
        # plt.figure()
        # plt.plot(self.iters, self.losses)
        # plt.savefig(log_dir+'loss.png',dpi=300)
        # plt.close()
        #
        # plt.figure()
        # plt.plot(self.iters, self.losses)
        # plt.ylim(0,1)
        # plt.savefig(log_dir+'loss_1.png',dpi=300)
        # plt.close()
        #
        # plt.figure()
        # plt.plot(self.iters, self.losses)
        # plt.ylim(0,0.1)
        # plt.savefig(log_dir+'loss_2.png',dpi=300)
        # plt.close()
        #
        # plt.figure()
        # plt.plot(self.iters, self.losses)
        # plt.ylim(0,0.01)
        # plt.savefig(log_dir+'loss_3.png',dpi=300)
        # plt.close()

class FcNet(Model):
    def build_model(self):
        CROP_DIMS   = self.config['CROP_DIMS']
        LEAK        = self.config['LEAK']
        C           = self.config['NUM_CHANNELS']
        LAMBDA      = self.config['L2_REG']
        INIT        = self.config['INIT']
        NUM_POINTS  = self.config['NUM_CONTOUR_POINTS']

        leaky_relu = tf.contrib.keras.layers.LeakyReLU(LEAK)

        self.x = tf.placeholder(shape=[None,CROP_DIMS,CROP_DIMS,C],dtype=tf.float32)
        self.y = tf.placeholder(shape=[None,NUM_POINTS],dtype=tf.float32)

        o = self.x
        if "INPUT_POOL" in self.config:
            d = self.config['INPUT_POOL']

            o = tf.nn.pool(o, [d,d], "MAX", "VALID", strides=[d,d])

        s = o.get_shape().as_list()

        o_vec = tf.reshape(o,shape=[-1,s[1]*s[2]*s[3]])

        for i,h in enumerate(self.config['HIDDEN_SIZES']):

            o_vec = tf_util.fullyConnected(o_vec, h,
                leaky_relu, std=INIT, scope='fc_'+str(i))

            if "DROPOUT" in self.config:
                o_vec = tf.nn.dropout(o_vec, self.config['DROPOUT'])

        self.yhat = tf_util.fullyConnected(o_vec, NUM_POINTS,
            tf.sigmoid, std=INIT, scope='fc_final')

        self.build_loss()

        self.saver = tf.train.Saver()

class I2INetReg(Model):
    def build_model(self):
        CROP_DIMS   = self.config['CROP_DIMS']
        C           = self.config['NUM_CHANNELS']
        LEAK        = self.config['LEAK']
        NUM_FILTERS = self.config['NUM_FILTERS']
        LAMBDA      = self.config['L2_REG']
        INIT        = self.config['INIT']
        NUM_POINTS  = self.config['NUM_CONTOUR_POINTS']

        leaky_relu = tf.contrib.keras.layers.LeakyReLU(LEAK)

        self.x = tf.placeholder(shape=[None,CROP_DIMS,CROP_DIMS,C],dtype=tf.float32)
        self.y = tf.placeholder(shape=[None,NUM_POINTS],dtype=tf.float32)

        self.yclass,self.yhat,_,_ = tf_util.I2INet(self.x,nfilters=NUM_FILTERS,
            activation=leaky_relu,init=INIT)

        o = leaky_relu(self.yhat)

        s = o.get_shape().as_list()

        o_vec = tf.reshape(o,shape=[-1,s[1]*s[2]*s[3]])

        for i in range(self.config['FC_LAYERS']-1):
            if "HIDDEN_SIZES" in self.config:
                h = self.config['HIDDEN_SIZES'][i]
            else:
                h = self.config['HIDDEN_SIZE']

            o_vec = tf_util.fullyConnected(o_vec, h,
                leaky_relu, std=INIT, scope='fc_'+str(i))

        self.yhat = tf_util.fullyConnected(o_vec, NUM_POINTS,
            tf.identity, std=INIT, scope='fc_final')

        self.build_loss()

        self.saver = tf.train.Saver()

class ResNetReg(Model):
    def build_model(self):
        CROP_DIMS   = self.config['CROP_DIMS']
        C           = self.config['NUM_CHANNELS']
        LEAK        = self.config['LEAK']
        LAMBDA      = self.config['L2_REG']
        INIT        = self.config['INIT']

        NLAYERS     = int(self.config['NLAYERS']/2)
        NFILTERS_SMALL = self.config['NFILTERS_SMALL']
        NFILTERS_LARGE = self.config['NFILTERS_LARGE']

        NUM_POINTS  = self.config['NUM_CONTOUR_POINTS']

        leaky_relu = tf.contrib.keras.layers.LeakyReLU(LEAK)

        self.x = tf.placeholder(shape=[None,CROP_DIMS,CROP_DIMS,C],dtype=tf.float32)
        self.y = tf.placeholder(shape=[None,NUM_POINTS],dtype=tf.float32)

        self.yclass,self.yhat,_,_ = tf_util.resNet(self.x,
            nlayers_before=NLAYERS, nlayers_after=NLAYERS,
            nfilters=NFILTERS_SMALL, nfilters_large=NFILTERS_LARGE,
            output_filters=NFILTERS_LARGE, activation=leaky_relu, init=INIT)

        o = leaky_relu(self.yhat)

        d = self.config['POOL']

        o = tf.nn.pool(o, [d,d], "MAX", "VALID", strides=[d,d])

        s = o.get_shape().as_list()

        o_vec = tf.reshape(o,shape=[-1,s[1]*s[2]*s[3]])

        for i in range(self.config['FC_LAYERS']):
            if "HIDDEN_SIZES" in self.config:
                h = self.config['HIDDEN_SIZES'][i]
            else:
                h = self.config['HIDDEN_SIZE']

            o_vec = tf_util.fullyConnected(o_vec, h,
                leaky_relu, std=INIT, scope='fc_'+str(i))

        self.yhat = tf_util.fullyConnected(o_vec, NUM_POINTS,
            tf.sigmoid, std=INIT, scope='fc_final')

        self.build_loss()

        self.saver = tf.train.Saver()

class ResNetRegMultiscale(Model):
    def build_model(self):
        CROP_DIMS   = self.config['CROP_DIMS']
        C           = self.config['NUM_CHANNELS']
        LEAK        = self.config['LEAK']
        LAMBDA      = self.config['L2_REG']
        INIT        = self.config['INIT']

        NLAYERS     = int(self.config['NLAYERS']/2)
        NFILTERS_SMALL = self.config['NFILTERS_SMALL']
        NFILTERS_LARGE = self.config['NFILTERS_LARGE']

        NUM_POINTS  = self.config['NUM_CONTOUR_POINTS']

        leaky_relu = tf.contrib.keras.layers.LeakyReLU(LEAK)

        self.x = tf.placeholder(shape=[None,CROP_DIMS,CROP_DIMS,C],dtype=tf.float32)

        if self.config['MULTI_TYPE'] == "POOL":
            self.x_1 = tf.nn.pool(self.x, [2,2], "MAX", "VALID", strides=[2,2])
            self.x_2 = tf.nn.pool(self.x_1, [2,2], "MAX", "VALID", strides=[2,2])
        elif self.config['MULTI_TYPE'] == "CROP":
            self.x_1 = tf.image.central_crop(self.x, central_fraction=0.5)
            self.x_2 = tf.image.central_crop(self.x_1, central_fraction=0.5)
        else:
            raise RuntimeError("Unrecognized multi type")

        self.y = tf.placeholder(shape=[None,NUM_POINTS],dtype=tf.float32)

        self.yclass,self.yhat,_,_ = tf_util.resNet(self.x,
            nlayers_before=NLAYERS, nlayers_after=NLAYERS,
            nfilters=NFILTERS_SMALL, nfilters_large=NFILTERS_LARGE,
            output_filters=NFILTERS_LARGE, activation=leaky_relu, init=INIT)

        self.yclass_1,self.yhat_1,_,_ = tf_util.resNet(self.x_1,
            nlayers_before=NLAYERS, nlayers_after=NLAYERS,
            nfilters=NFILTERS_SMALL, nfilters_large=NFILTERS_LARGE,
            output_filters=NFILTERS_LARGE, activation=leaky_relu, init=INIT,
            scope="resnet_1")

        self.yclass_2,self.yhat_2,_,_ = tf_util.resNet(self.x_2,
            nlayers_before=NLAYERS, nlayers_after=NLAYERS,
            nfilters=NFILTERS_SMALL, nfilters_large=NFILTERS_LARGE,
            output_filters=NFILTERS_LARGE, activation=leaky_relu, init=INIT,
            scope="resnet_2")


        o   = leaky_relu(self.yhat)
        o_1 = leaky_relu(self.yhat_1)
        o_2 = leaky_relu(self.yhat_2)

        s   = o.get_shape().as_list()
        s_1 = o_1.get_shape().as_list()
        s_2 = o_2.get_shape().as_list()

        o_vec   = tf.reshape(o,shape=[-1,s[1]*s[2]*s[3]])
        o_vec_1 = tf.reshape(o_1,shape=[-1,s_1[1]*s_1[2]*s_1[3]])
        o_vec_2 = tf.reshape(o_2,shape=[-1,s_2[1]*s_2[2]*s_2[3]])

        o = tf.concat([o_vec, o_vec_1, o_vec_2], axis=1)

        print(o)

        for i in range(self.config['FC_LAYERS']-1):
            if "HIDDEN_SIZES" in self.config:
                h = self.config['HIDDEN_SIZES'][i]
            else:
                h = self.config['HIDDEN_SIZE']

            o = tf_util.fullyConnected(o, h,
                leaky_relu, std=INIT, scope='fc_'+str(i))

        self.yhat = tf_util.fullyConnected(o, NUM_POINTS,
            tf.identity, std=INIT, scope='fc_final')

        self.build_loss()

        self.saver = tf.train.Saver()

class ConvNet(Model):
    def build_model(self):
        CROP_DIMS   = self.config['CROP_DIMS']
        C           = self.config['NUM_CHANNELS']
        LEAK        = self.config['LEAK']
        LAMBDA      = self.config['L2_REG']
        INIT        = self.config['INIT']

        NLAYERS     = self.config['NLAYERS']
        NFILTERS    = self.config['NFILTERS']

        NUM_POINTS  = self.config['NUM_CONTOUR_POINTS']
        DIMS = [self.config['CONV_DIMS']]*2

        leaky_relu = tf.contrib.keras.layers.LeakyReLU(LEAK)

        self.x = tf.placeholder(shape=[None,CROP_DIMS,CROP_DIMS,C],dtype=tf.float32)
        self.y = tf.placeholder(shape=[None,NUM_POINTS],dtype=tf.float32)

        o = self.x

        if "INPUT_POOL" in self.config:
            d = self.config['INPUT_POOL']

            o = tf.nn.pool(o, [d,d], "MAX", "VALID", strides=[d,d])


        for i in range(NLAYERS):
            o = tf_util.conv2D(o,dims=DIMS,nfilters=NFILTERS,
                               init=INIT,
                          activation=leaky_relu,scope="conv_{}".format(i))

        s   = o.get_shape().as_list()
        o = tf.reshape(o,shape=[-1,s[1]*s[2]*s[3]])

        for i in range(self.config['FC_LAYERS']):
            if "HIDDEN_SIZES" in self.config:
                h = self.config['HIDDEN_SIZES'][i]
            else:
                h = self.config['HIDDEN_SIZE']

            o = tf_util.fullyConnected(o, h,
                leaky_relu, std=INIT, scope='fc_'+str(i))
            print(o)

            if "DROPOUT" in self.config:
                o = tf.nn.dropout(o, self.config['DROPOUT'])


        self.yhat = tf_util.fullyConnected(o, NUM_POINTS,
            tf.sigmoid, std=INIT, scope='fc_final')

        self.build_loss()

        self.saver = tf.train.Saver()

class ConvNetMulti(Model):
    def build_model(self):
        CROP_DIMS   = self.config['CROP_DIMS']
        C           = self.config['NUM_CHANNELS']
        LEAK        = self.config['LEAK']
        LAMBDA      = self.config['L2_REG']
        INIT        = self.config['INIT']

        NLAYERS     = int(self.config['NLAYERS']/2)
        NFILTERS    = self.config['NFILTERS']

        NUM_POINTS  = self.config['NUM_CONTOUR_POINTS']
        DIMS = [self.config['CONV_DIMS']]*2

        leaky_relu = tf.contrib.keras.layers.LeakyReLU(LEAK)

        self.x = tf.placeholder(shape=[None,CROP_DIMS,CROP_DIMS,C],dtype=tf.float32)
        self.y = tf.placeholder(shape=[None,NUM_POINTS],dtype=tf.float32)

        self.x_1 = tf.nn.pool(self.x, [2,2], "MAX", "VALID", strides=[2,2])
        self.x_2 = tf.nn.pool(self.x_1, [2,2], "MAX", "VALID", strides=[2,2])

        o = self.x

        for i in range(NLAYERS):
            o = tf_util.conv2D(o,dims=DIMS, nfilters=NFILTERS,init=INIT,activation=leaky_relu,
                scope="conv_{}".format(i))

        o_1 = self.x_1

        for i in range(NLAYERS):
            o_1 = tf_util.conv2D(o_1,dims=DIMS, nfilters=NFILTERS,init=INIT,activation=leaky_relu, scope="conv_1_{}".format(i))

        o_2 = self.x_2

        for i in range(NLAYERS):
            o_2 = tf_util.conv2D(o_2,dims=DIMS, nfilters=NFILTERS,init=INIT,activation=leaky_relu, scope="conv_2_{}".format(i))


        s = o.get_shape().as_list()
        s_1 = o_1.get_shape().as_list()
        s_2 = o_2.get_shape().as_list()

        o_vec   = tf.reshape(o,shape=[-1,s[1]*s[2]*s[3]])
        o_vec_1 = tf.reshape(o_1,shape=[-1,s_1[1]*s_1[2]*s_1[3]])
        o_vec_2 = tf.reshape(o_2,shape=[-1,s_2[1]*s_2[2]*s_2[3]])

        o = tf.concat([o_vec, o_vec_1, o_vec_2], axis=1)

        for i in range(self.config['FC_LAYERS']-1):
            if "HIDDEN_SIZES" in self.config:
                h = self.config['HIDDEN_SIZES'][i]
            else:
                h = self.config['HIDDEN_SIZE']

            o = tf_util.fullyConnected(o, h,
                leaky_relu, std=INIT, scope='fc_'+str(i))

        self.yhat = tf_util.fullyConnected(o, NUM_POINTS,
            tf.identity, std=INIT, scope='fc_final')

        self.build_loss()

        self.saver = tf.train.Saver()

class GoogleNet(Model):
    def build_model(self):
        CROP_DIMS   = self.config['CROP_DIMS']
        C           = self.config['NUM_CHANNELS']
        LEAK        = self.config['LEAK']
        LAMBDA      = self.config['L2_REG']
        INIT        = self.config['INIT']
        DROPOUT     = self.config['DROPOUT']
        NUM_POINTS  = self.config['NUM_CONTOUR_POINTS']

        leaky_relu = tf.contrib.keras.layers.LeakyReLU(LEAK)

        self.x = tf.placeholder(shape=[None,CROP_DIMS,CROP_DIMS,C],dtype=tf.float32)
        self.y = tf.placeholder(shape=[None,NUM_POINTS],dtype=tf.float32)

        o,o_side = tf_util.GoogleNet(self.x, activation=leaky_relu, init=INIT,
            scope='googlenet', output_size=NUM_POINTS, dropout=DROPOUT)

        print(o)
        print(o_side)

        self.yhat = tf.nn.sigmoid(o)
        self.yhat_side = tf.nn.sigmoid(o_side)

        self.build_loss()

        self.saver = tf.train.Saver()

        self.dropout_mask_op = op = tf.get_default_graph().get_tensor_by_name(
            "googlenet/dropout_1/random_uniform:0")

        self.dropout_mask = None
        self.dropout_fixed = False

    def build_loss(self):
        self.loss = tf.reduce_mean(tf.square(self.y-self.yhat))
        self.loss += 0.3*tf.reduce_mean(tf.square(self.y-self.yhat_side))

    def sample(self):
        self.dropout_mask = (np.random.uniform(size=(1,9216))<=0.6).astype(int)
        self.dropout_fixed = True

    def _predict(self,x):
        if not self.dropout_fixed:
            return self.sess.run(self.yhat,{self.x:x})
        else:
            return self.sess.run(self.yhat,{self.x:x,
                self.dropout_mask_op:self.dropout_mask})
