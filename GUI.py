import tkinter as tk
from tkinter import filedialog
from tkinter.font import Font
from PIL import ImageTk, Image
import subprocess, os, threading, sys


class MainWindow(tk.Frame):
	def __init__(self, master=None):
		super().__init__(master)
		self.master = master
		self.grid()
		self.create_gui()
		self.r_thread = None
	
	def create_gui(self):
		
		# TODO: delete these paths after testing
		o_path = "/Users/Shea/Desktop/COMP 4910/RGB Data/rgb_map.tif"
		pc_path = "/Users/Shea/Desktop/COMP 4910/RGB Data/points.las"
		
		font = Font(family="Helvetica", size=26, weight="bold")
		
		# add image background
		self.image = Image.open("forest_bg copy.png")
		self.background_image = ImageTk.PhotoImage(self.image)
		self.background_label = tk.Label(self.master, image=self.background_image)
		self.background_label.place(x=0, y=0, relwidth=1, relheight=1)
		self.master.geometry("{}x{}".format(self.image.size[0], self.image.size[1]))
		
		# create containers for individual rows
		title_container = tk.Frame(root, bg="")
		browse_container = tk.Frame(root, bg="")
		tog_container = tk.Frame(root, bg="")
		in_container = tk.Frame(root, bg="")
		btn_container = tk.Frame(root, bg="")
		text_container = tk.Frame(root)
		
		# create title
		self.title = tk.Label(title_container, text="Tree Identification Software v2", font=font)
		self.title.grid(row=0, column=0, sticky="nsew")
		
		# create file browsers
		self.ortho_label = tk.Label(browse_container, text="Orthomosaic:")
		self.ortho_path = tk.Label(browse_container, text=o_path)
		# self.ortho_path = tk.Label(browse_container, text="No Orthomosaic (.TIF) selected", bg="systemTransparent")
		self.ortho_button = tk.Button(browse_container, text="Browse", command=self.browse_ortho)
		self.ortho_label.grid(row=0, column=0, sticky="nsew")
		self.ortho_path.grid(row=0, column=1, sticky="nsew")
		self.ortho_button.grid(row=0, column=2, sticky="nsew")
		
		self.pc_label = tk.Label(browse_container, text="Point Cloud:")
		self.pc_path = tk.Label(browse_container, text=pc_path)
		# self.pc_path = tk.Label(browse_container, text="No Point Cloud (.LAS) selected", bg="systemTransparent")
		self.pc_button = tk.Button(browse_container, text="Browse", command=self.browse_pc)
		self.pc_label.grid(row=1, column=0, sticky="nsew")
		self.pc_path.grid(row=1, column=1, sticky="nsew")
		self.pc_button.grid(row=1, column=2, sticky="nsew")
		
		# create plot toggles
		self.toggle_label = tk.Label(tog_container, text="Output Plots:")
		self.toggle_label.grid(row=1, column=0, columnspan=8, sticky="nsew")
		
		# 2D Cross section
		self.cross_var = tk.BooleanVar(value=False)
		self.cross_button = tk.Checkbutton(tog_container, text="Cross", variable=self.cross_var)
		self.cross_button.grid(row=2, column=0)
		
		# 3d Digital terrain model
		self.dtm_var = tk.BooleanVar(value=False)
		self.dtm_button = tk.Checkbutton(tog_container, text="DTM", variable=self.dtm_var)
		self.dtm_button.grid(row=2, column=1)
		
		# 3D Normalized (flat ground) point cloud. Heat Map colouring
		self.n_normalized_var = tk.BooleanVar(value=False)
		self.n_normalized_button = tk.Checkbutton(tog_container, text="Non-norm", variable=self.n_normalized_var)
		self.n_normalized_button.grid(row=2, column=2)
		
		# 3D Normalized (flat ground) point cloud. Heat Map colouring
		self.normalized_var = tk.BooleanVar(value=False)
		self.normalized_button = tk.Checkbutton(tog_container, text="Norm", variable=self.normalized_var)
		self.normalized_button.grid(row=2, column=3)
		
		# 3D rgb point cloud
		self.rgb_var = tk.BooleanVar(value=False)
		self.rgb_button = tk.Checkbutton(tog_container, text="RGB", variable=self.rgb_var)
		self.rgb_button.grid(row=2, column=4)
		
		# 2D located trees
		self.canopy_var = tk.BooleanVar(value=False)
		self.canopy_button = tk.Checkbutton(tog_container, text="Canopy", variable=self.canopy_var)
		self.canopy_button.grid(row=2, column=5)
		
		# 3D segments
		self.segments_var = tk.BooleanVar(value=False)
		self.segments_button = tk.Checkbutton(tog_container, text="Segments", variable=self.segments_var)
		self.segments_button.grid(row=2, column=6)
		
		# 2D segments - rgb overlay
		self.overlay_var = tk.BooleanVar(value=False)
		self.overlay_button = tk.Checkbutton(tog_container, text="Overlay", variable=self.overlay_var)
		self.overlay_button.grid(row=2, column=7)
		
		# create inputs
		self.toggle_label = tk.Label(in_container, text="Point Cloud Tile Params:")
		self.toggle_label.grid(row=0, column=0, columnspan=2, sticky="nsew")
		
		self.tile_input_var = tk.StringVar(value="250")
		self.tile_size_label = tk.Label(in_container, text="Tile Size (m):")
		self.tile_size_label.grid(row=3, column=0)
		self.tile_size_entry = tk.Entry(in_container, textvariable=self.tile_input_var)
		self.tile_size_entry.grid(row=3, column=1)
		
		self.buff_input_var = tk.StringVar(value="0")
		self.tile_buffer_label = tk.Label(in_container, text="Tile Buffer (m):")
		self.tile_buffer_label.grid(row=4, column=0)
		self.tile_buffer_entry = tk.Entry(in_container, textvariable=self.buff_input_var)
		self.tile_buffer_entry.grid(row=4, column=1)
		
		# create options for tables
		# TODO: options for outputting 2 column table for manual labelling, etc
		
		# create run/ stop button
		self.run_button = tk.Button(btn_container, text="Run", command=self.run_segmentation)
		self.run_button.grid(row=0, column=1)
		self.stop_button = tk.Button(btn_container, text="Stop", command=self.stop_r_thread)
		self.stop_button.grid(row=0, column=2)
		
		# create output text box
		self.toggle_button = tk.Button(text_container, text="Show Output", command=self.toggle_textbox)
		self.toggle_button.grid(row=0, column=0)
		self.textbox = tk.Text(text_container, height=15)
		self.textbox.grid(row=1, column=0)
		
		# place containers on GUI
		title_container.place(relx=0.5, rely=0.2, anchor="center")
		browse_container.place(relx=0.5, rely=0.3, anchor="center")
		tog_container.place(relx=0.5, rely=0.4, anchor="center")
		in_container.place(relx=0.5, rely=0.53, anchor="center")
		btn_container.place(relx=0.5, rely=0.63, anchor="center")
		text_container.place(relx=0.5, rely=0.9, anchor="center")
		self.textbox.grid_remove()
		
	def browse_ortho(self):
		self.ortho_p = filedialog.askopenfilename()
		self.ortho_path.config(text=self.ortho_p)
	
	def browse_pc(self):
		self.pc_p = filedialog.askopenfilename()
		self.pc_path.config(text=self.pc_p)
		
	def toggle_textbox(self):
		# toggle the visibility of the text box
		if self.textbox.winfo_ismapped():
			self.textbox.grid_remove()
			self.toggle_button.config(text="Show Output")
		else:
			self.textbox.grid()
			self.toggle_button.config(text="Hide")
	
	def stop_r_thread(self):
		if self.r_thread is not None:
			self.r_thread.stop_flag.set()
			self.run_button.config(state="normal")
	
	def run_segmentation(self):
		self.run_button.config(state="disabled")
		self.textbox.delete("1.0", tk.END)
		self.textbox.grid()
		# Get the input values from the GUI
		# Paths
		ortho_path = self.ortho_path.cget("text")
		pc_path = self.pc_path.cget("text")
		# Inputs
		tile_size = int(self.tile_size_entry.get())
		tile_buffer = int(self.tile_buffer_entry.get())
		# Toggles
		cross = self.cross_var.get()
		dtm = self.dtm_var.get()
		n_normalized = self.n_normalized_var.get()
		normalized = self.normalized_var.get()
		rgb = self.rgb_var.get()
		canopy = self.canopy_var.get()
		segments = self.segments_var.get()
		overlay = self.overlay_var.get()
		
		# Input checks before running script
		# Checks to do:
		# - extreme tile/ buffer sizes
		# - file type of input files
		# - No plots selected
		
		if tile_size < 1 or tile_buffer < 0:
			# Display an error message
			tk.messagebox.showerror("Error",
			                        "Tile size must be greater than 0.\nBuffer size must be greater than or equal to 0")
			return
		if ortho_path == "No Orthomosaic (.TIF) selected":
			# Display an error message
			tk.messagebox.showerror("Error", "Orthomosaic file not selected")
			return
		if pc_path == "No Point Cloud (.LAS) selected":
			# Display an error message
			tk.messagebox.showerror("Error", "Point Cloud file not selected")
			return
		
		else:
			# Set the command line arguments for the R script
			r_script = "./tree_segmentation_v2.R"
			
			# Make the R script executable (equivalent to running "chmod +x r_script" in terminal)
			os.chmod(r_script, 0o755)
			
			# args = ["Rscript", r_script, str(ortho_path), str(pc_path), str(tile_size), str(tile_buffer),
			#         str(dtm), str(normalized), str(canopy), str(segments), str(rgb)]
			
			cmd = ["Rscript", r_script]
			args = [str(ortho_path), str(pc_path), str(tile_size), str(tile_buffer), str(cross), str(dtm),
			        str(n_normalized), str(normalized), str(rgb), str(canopy), str(segments), str(overlay)]
			
			# Create RThread and start it
			self.r_thread = RThread(cmd, self.textbox, args=args)
			self.r_thread.start()


class RThread(threading.Thread):
	def __init__(self, cmd, text_box, args=None):
		threading.Thread.__init__(self)
		self.cmd = cmd
		self.text_box = text_box
		self.args = args
		self.stop_flag = threading.Event()
	
	def run(self):
		cmd = self.cmd + self.args if self.args else self.cmd
		try:
			process = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
			
			# # Run the R script using subprocess
			# try:
			# 	# subprocess.run(args)
			# 	# output = subprocess.check_output(args, stderr=subprocess.STDOUT)
			# 	# print(output.decode())
			# 	process = subprocess.Popen(["Rscript", r_script, *args], stdout=subprocess.PIPE,
			# 	                           stderr=subprocess.STDOUT, universal_newlines=True)
			#
			# 	# Read the output line by line and print it to the console
			# 	for line in process.stdout:
			# 		print(line, end='')
			#
			# 		self.textbox.insert(tk.END, line)
			#
			# 	# TODO: get text stream working properly
			# 	# # read the output from the pipe
			# 	# output = proc.stdout.read()
			# 	#
			# 	# # insert the output into the text box
			# 	# self.textbox.insert(tk.END, output)
			#
			# 	# output, _ = process.communicate()
			# 	# self.textbox.insert("1.0", output) # "1.0" to add to top, tk.END to add to bottom
			# 	# self.textbox.see(tk.END)
			# 	# print(output, end='')
			# 	#
			# 	# # continuously read the output from the pipe and insert it into the text box
			# 	# for line in iter(process.stdout.readline, ""):
			# 	# 	self.textbox.insert(tk.END, line)
			# 	# 	self.textbox.see(tk.END)
			# 	# 	self.parent.update_idletasks()
			# 	#
			# 	#
			# 	# process.stdout.close()
			# 	# process.wait()
			#
			#
			# except subprocess.CalledProcessError as e:
			# 	print("Error:", e.output.decode())
			
			while True:
				output = process.stdout.readline() + process.stderr.readline()
				if output == b'' and process.poll() is not None:
					break
				if output:
					print(output)
					self.text_box.insert("1.0", output)
				if self.stop_flag.is_set():
					process.terminate()
					text = "Process terminated by user"
					self.text_box.insert("1.0", text)
					break
			rc = process.poll()
			return rc
		
		except subprocess.CalledProcessError as e:
			print("Error:", e.output.decode())


if __name__ == "__main__":
	root = tk.Tk()
	root.title("Aurora UAV: Tree ID v2")
	
	app = MainWindow(master=root)
	# app.pack(fill=tk.BOTH, expand=True)
	app.mainloop()



