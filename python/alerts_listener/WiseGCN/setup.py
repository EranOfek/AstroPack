from distutils.core import setup
import setuptools

setup(name='wisegcn',
      version='0.1.0',
      author="Na'ama Hallakoun",
      author_email='naama@wise.tau.ac.il',
      description='Process GCN LVC alerts for follow-up at the Wise Observatory',
      long_description=open('README.md').read(),
      url='https://github.com/naamach/wisegcn',
      license='LICENSE.txt',
      packages=setuptools.find_packages(),
      install_requires=['pygcn', 'healpy', 'configparser', 'astropy', 'pymysql', 'voevent-parse', 'numpy', 'scipy',
                        'requests', 'lxml', 'ccdproc'],
      classifiers=[
          'Development Status :: 4 - Beta',
          'Environment :: Console',
          'Intended Audience :: Science/Research',
          'License :: OSI Approved :: GNU General Public License v3 or later (GPLv3+)',
          'Programming Language :: Python',
          'Topic :: Scientific/Engineering :: Astronomy',
          'Topic :: Text Processing :: Markup :: XML'
      ],
      scripts=['bin/wisegcn-listen', 'bin/wisegcn-ingest']
      )
