import os
import numpy as np
import tempfile
import datetime
import pydicom
from pydicom.dataset import Dataset, FileDataset
from matplotlib import pyplot

def normalize(v):
    norm = np.amax(v)
    #print "Norm: ", norm
    #print "Max: ", np.amax(v)
    if norm == 0: 
       return v
    result= v*100 / norm
    #print "Max: ", np.amax(result)
    return result
print '********************************************************************************'
print '                    Setting file information and dataset values'
print '********************************************************************************'
print ''
# Populate required values for file meta information
file_meta = Dataset()
file_meta.MediaStorageSOPClassUID = 'RT Image Storage'
file_meta.MediaStorageSOPInstanceUID = "1.2.826.0.1.3680043.8.937.1.20170926.8691"
file_meta.ImplementationClassUID = "1.2.3.4"
print("Setting dataset values...")
# Create the FileDataset instance (initially no data elements, but file_meta
# supplied)
ds = Dataset(file_meta=file_meta, preamble=b"\0" * 128)

# Set the transfer syntax
ds.is_little_endian = True
ds.is_implicit_VR = True

# Set creation date/time
ds.SOPClassUID = 'RT Image Storage'
ds.SOPInstanceUID = "1.2.826.0.1.3680043.8.937.1.20170926.8691"
ds.ImageType=['ORIGINAL', 'PRIMARY', 'PORTAL', '']
dt = datetime.datetime.now()
ds.StudyDate = dt.strftime('%Y%m%d')
ds.SeriesDate = dt.strftime('%Y%m%d')
ds.AcquisitionDate = dt.strftime('%Y%m%d')
ds.ContentDate = dt.strftime('%Y%m%d')
timeStr = dt.strftime('%H%M%S.%f')  # long format with micro seconds
ds.StudyTime = timeStr
ds.SeriesTime = timeStr
ds.AcquisitionTime = timeStr
ds.ContentTime = timeStr
ds.AccessionNumber =''
ds.Modality = 'RTImage'
ds.ConversionType = 'DI'
ds.Manufacturer="GATE"
ds.ReferringPhysiciansName = ''
ds.StationName="GATE Dose Actor"
ds.PatientName = "Phantom name"
#ds.PatientName = raw_input('Give your DICOM a name: ')
ds.PatientID = "123456"
ds.PatientBirthDate=''
ds.PatientSex=''
ds.DeviceSerialNumber='9101060'
ds.StudyInstanceUID='1.2.826.0.1.3680043.2.1125.1.35859627302800520295369011388644332'
ds.SeriesInstanceUID='1.2.826.0.1.3680043.2.1125.1.81507923164692601234912200876243257'
ds.StudyID=''
ds.SeriesNumber=''
ds.InstanceNumber=''
ds.FrameOfReferenceUID='1.2.826.0.1.3680043.2.1125.1.22504207008437831324334424494191176'
ds.SamplesPerPixel=1
ds.PhotometricInterpretation='MONOCHROME2'
ds.Rows=600
ds.Columns=600
print ''
print 'Set the row and column number for your output DICOM. The default is set to the Lynx 600x600 grid.'
print 'For GATE simulations you will find the dimensions in the header of your .dat-file.'
print ''
ds.Rows=int(raw_input("Row number (y-direction): "))
ds.Columns=int(raw_input("Column number (x-direction): "))
print ''
print 'If you want to set the pixel spacing, uncomment the lines following this print command in the python file. It is set to the Lynx standard of (0.5,0.5)mm.'
print ''
#PixelSpacing=raw_input('Pixel spacing: ')
#ds.PixelSpacing =[PixelSpacing,PixelSpacing]
#ds.ImagePlanePixelSpacing =[PixelSpacing,PixelSpacing]
ds.PixelSpacing=[0.5,0.5]
ds.ImagePlanePixelSpacing=[0.5,0.5]
ds.BitsAllocated=16
ds.BitsStored=16
ds.HighBit=15
ds.PixelRepresentation=0
ds.RTImageLabel="GATE output"
ds.ReportedValuesOrigin='OPERATOR'
ds.RTImagePlane='NORMAL'
ds.XRayImageReceptorAngle=''
posX=-1.0*ds.PixelSpacing[0]*ds.Columns/2.0
posY=-1.0*ds.PixelSpacing[1]*ds.Rows/2.0
ds.RTImagePosition= [posX,posY]
ds.RadiationMachineName=''
ds.RadiationMachineSAD="1000"
ds.RadiationMachineSSD="0"
ds.RTImageSID="1000"
ds.PrimaryDosimeterUnit=''
print '********************************************************************************'
print '                           Now creating pixel array'
print '********************************************************************************'
print ''
dir_path = os.path.dirname(os.path.realpath(__file__))
dirs= os.listdir(dir_path) 
print 'Locate simulation file\nIn this folder there are: \n'
for file in dirs:
   print file

simpath=raw_input("\nPut in folder containing the data \n")

print "\nLoad ascii data\nIn this folder there are: \n"
dirs= os.listdir(dir_path + '/' + simpath) 
for file in dirs:
   print file
filename = raw_input("\nEnter ascii file name without ending: ")
simfile = simpath + '/' + filename +".dat"
print "Open file: ", simfile
z=np.loadtxt(simfile, skiprows=6, dtype="float")
z=normalize(z)
# To check normalisation
#print "Max: ", np.amax(z)

print 'To print ascii data uncomment line in python file.'

# To print the ascii data
#print z

ConstPixelDims = (int(ds.Rows), int(ds.Columns))
ConstPixelSpacing = (float(ds.PixelSpacing[0]), float(ds.PixelSpacing[1]))
x = np.arange(-150.0,(ConstPixelDims[0]+1)*ConstPixelSpacing[0]/2, ConstPixelSpacing[0])
y = np.arange(-150.0,(ConstPixelDims[1]+1)*ConstPixelSpacing[1]/2, ConstPixelSpacing[1])
# The array is sized based on 'ConstPixelDims'
ArrayDicom = np.zeros(ConstPixelDims, dtype="uint16")
# store the raw image data
ArrayDicom = z.astype(np.uint16) 

# To show the image stored in the .dat file
#print "\nShow ascii as image\n"
#pyplot.figure(dpi=150)
#pyplot.axes().set_aspect('equal', 'datalim')
#pyplot.set_cmap(pyplot.gray())
##pyplot.pcolormesh(x, y, np.flipud(ArrayDicom[:, :]))
#pyplot.pcolormesh(ArrayDicom)
#pyplot.show()

ds.PixelData = ArrayDicom
#print 'Pixel array data type: ', ds.pixel_array.dtype

# To print the ascii data copied into the DICOM-file
#print ds.PixelData
# To print the whole dataset
#print ds

print ''
print '********************************************************************************'
print '                                Write file'
print '********************************************************************************'
print ''
savepath = simpath + '/' + filename +".dcm"
print savepath
ds.save_as(savepath)
print "File saved."
print "Type \n vv ", savepath, " \nto open the ouput DICOM file.\n" 