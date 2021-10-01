 from setuptools import setup
 
 setup(
   name='sandbox',
   version='0.0',
   packages=['sandbox', 'sandbox.test', 'sandbox/ooi_pioneer'],
   url='https://github.com/IvanaEscobar/sandbox',
   keywords=['ocean','computation'],
   license='LICENSE.txt',
   description='Rough Draft Code Package',
   long_description=open('README.txt').read(),
   long_description_content_type='text/markdown'
   install_requires=[
       "numpy >= 1.19.1",
       "pandas",
       "xarray",
       "xmitgcm",
       "ecco_v4_py"
   ],
   author='Ivana Escobar',
   author_email='ivana@utexas.edu'
)
