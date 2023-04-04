import tkinter as tk
from tkinter import filedialog
from PIL import ImageTk, Image
import subprocess


class MainWindow(tk.Frame):
	def __init__(self, master=None):
		super().__init__(master)
		self.master = master
		self.grid()
		self.create_gui()
	
	def create_gui(self):
		
		# add image background
		self.image = Image.open("forest_bg copy.png")
		self.background_image = ImageTk.PhotoImage(self.image)
		self.background_label = tk.Label(self.master, image=self.background_image)
		self.background_label.place(x=0, y=0, relwidth=1, relheight=1)
		self.master.geometry("{}x{}".format(self.image.size[0], self.image.size[1]))
		
		# create containers for individual rows
		browse_container = tk.Frame(root, bg="")
		tog_container = tk.Frame(root, bg="")
		in_container = tk.Frame(root, bg="")
		btn_container = tk.Frame(root, bg="")
		
		# create file browsers
		self.ortho_label = tk.Label(browse_container, text="Orthomosaic:", bg="systemTransparent")
		self.ortho_path = tk.Label(browse_container, text="No Orthomosaic (.TIF) selected", bg="systemTransparent")
		self.ortho_button = tk.Button(browse_container, text="Browse", command=self.browse_ortho, bg="systemTransparent")
		self.ortho_label.grid(row=0, column=0, sticky="nsew")
		self.ortho_path.grid(row=0, column=1, sticky="nsew")
		self.ortho_button.grid(row=0, column=2, sticky="nsew")
		
		self.pc_label = tk.Label(browse_container, text="Point Cloud:", bg="systemTransparent")
		self.pc_path = tk.Label(browse_container, text="No Point Cloud (.LAS) selected", bg="systemTransparent")
		self.pc_button = tk.Button(browse_container, text="Browse", command=self.browse_pc, bg="systemTransparent")
		self.pc_label.grid(row=1, column=0, sticky="nsew")
		self.pc_path.grid(row=1, column=1, sticky="nsew")
		self.pc_button.grid(row=1, column=2, sticky="nsew")
		
		# create plot toggles
		self.toggle_label = tk.Label(tog_container, text="Output Plots:", bg="systemTransparent")
		self.toggle_label.grid(row=1, column=0, columnspan=5, sticky="nsew")
		
		self.dtm_var = tk.BooleanVar(value=False)
		self.dtm_button = tk.Checkbutton(tog_container, text="DTM", variable=self.dtm_var, bg="systemTransparent")
		self.dtm_button.grid(row=2, column=0)
		
		self.normalized_var = tk.BooleanVar(value=False)
		self.normalized_button = tk.Checkbutton(tog_container, text="Normalized", variable=self.normalized_var, bg="systemTransparent")
		self.normalized_button.grid(row=2, column=1)
		
		self.canopy_var = tk.BooleanVar(value=False)
		self.canopy_button = tk.Checkbutton(tog_container, text="Canopy", variable=self.canopy_var, bg="systemTransparent")
		self.canopy_button.grid(row=2, column=2)
		
		self.segments_var = tk.BooleanVar(value=False)
		self.segments_button = tk.Checkbutton(tog_container, text="Segments", variable=self.segments_var, bg="systemTransparent")
		self.segments_button.grid(row=2, column=3)
		
		self.rgb_var = tk.BooleanVar(value=False)
		self.rgb_button = tk.Checkbutton(tog_container, text="RGB", variable=self.rgb_var, bg="systemTransparent")
		self.rgb_button.grid(row=2, column=4)
		
		# create inputs
		self.tile_input_var = tk.StringVar(value="250")
		self.tile_size_label = tk.Label(in_container, text="Tile Size (m):", bg="systemTransparent")
		self.tile_size_label.grid(row=3, column=0)
		self.tile_size_entry = tk.Entry(in_container, textvariable=self.tile_input_var, bg="systemTransparent")
		self.tile_size_entry.grid(row=3, column=1)
		
		self.buff_input_var = tk.StringVar(value="0")
		self.tile_buffer_label = tk.Label(in_container, text="Tile Buffer (m):", bg="systemTransparent")
		self.tile_buffer_label.grid(row=4, column=0)
		self.tile_buffer_entry = tk.Entry(in_container, textvariable=self.buff_input_var, bg="systemTransparent")
		self.tile_buffer_entry.grid(row=4, column=1)
		
		# create options for tables
		# TODO: options for outputting 2 column table for manual labelling, etc
		
		# create run button
		run_button = tk.Button(btn_container, text="Run", command=self.run_segmentation, bg="systemTransparent")
		run_button.grid(row=5, column=1)
		
		# place container on
		browse_container.place(relx=0.5, rely=0.3, anchor="center")
		tog_container.place(relx=0.5, rely=0.4, anchor="center")
		in_container.place(relx=0.5, rely=0.5, anchor="center")
		btn_container.place(relx=0.5, rely=0.6, anchor="center")
		
	def browse_ortho(self):
		self.ortho_p = filedialog.askopenfilename()
		self.ortho_path.config(text=self.ortho_p)
	
	def browse_pc(self):
		self.pc_p = filedialog.askopenfilename()
		self.pc_path.config(text=self.pc_p)
	
	def run_segmentation(self):
		# Get the values from the GUI
		ortho_path = self.ortho_path.cget("text")
		pc_path = self.pc_path.cget("text")
		tile_size = int(self.tile_size_entry.get())
		tile_buffer = int(self.tile_buffer_entry.get())
		normalize = self.normalized_var.get()
		
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
			# args = ["Rscript", "tree_segmentation_v1.R", ortho_path, str(tile_size), str(normalize)]
			print("would run R script")
			
			# Run the R script using subprocess
			# try:
			# 	output = subprocess.check_output(args, stderr=subprocess.STDOUT)
			# 	print(output.decode())
			# except subprocess.CalledProcessError as e:
			# 	print("Error:", e.output.decode())


if __name__ == "__main__":
	root = tk.Tk()
	root.title("Aurora UAV: Tree ID v1")
	
	app = MainWindow(master=root)
	# app.pack(fill=tk.BOTH, expand=True)
	app.mainloop()



