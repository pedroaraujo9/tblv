from upload_to_google_drive_utils import upload_to_drive
import sys 

print("Arguments passed to the script:")
for i, arg in enumerate(sys.argv):
    print(f"Argument {i}: {arg}")

file_path = sys.argv[1]
drive_file_name = sys.argv[2]
drive_folder_id = sys.argv[3]

file_up_id = upload_to_drive(drive_file_name, file_path, drive_folder_id)
print(f'Uploaded file with ID: {file_up_id}')
